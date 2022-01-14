{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Maintainer: Sean Whitton <spwhitton@spwhitton.name>

Build and maintain schroots for use with sbuild.

For convenience we set up several enhancements, such as ccache and eatmydata.
This means we have to make several assumptions:

1. you want to build for a Debian release strictly newer than squeeze, or for a
Buntish release newer than or equal to trusty

2. if you want to build for Debian stretch or newer, you have sbuild 0.70.0 or
newer

The latter is due to the migration from GnuPG v1 to GnuPG v2.1 in Debian
stretch, which older sbuild can't handle.

Suggested usage in @config.hs@:

>  mybox = host "mybox.example.com" $ props
>  	& osDebian (Stable "buster") X86_64
>  	& Apt.useLocalCacher
>  	& sidSchrootBuilt
>  	& Sbuild.usableBy (User "spwhitton")
>  	& Schroot.overlaysInTmpfs
>    where
>  	sidSchrootBuilt = Sbuild.built Sbuild.UseCcache $ props
>  		& osDebian Unstable X86_32
>  		& Sbuild.osDebianStandard
>  		& Sbuild.update `period` Weekly (Just 1)
>  		& Chroot.useHostProxy mybox

If you are using sbuild older than 0.70.0, you also need:

>  & Sbuild.keypairGenerated

To take advantage of the piuparts and autopkgtest support, add to your
@~/.sbuildrc@ (assumes sbuild 0.71.0 or newer):

>  $piuparts_opts = [
>      '--no-eatmydata',
>      '--schroot',
>      '%r-%a-sbuild',
>      '--fail-if-inadequate',
>      ];
>
>  $autopkgtest_root_args = "";
>  $autopkgtest_opts = ["--", "schroot", "%r-%a-sbuild"];

On Debian jessie hosts, you should ensure that sbuild and autopkgtest come from
the same suite.  This is because the autopkgtest binary changed its name between
jessie and stretch.  If you have not installed backports of sbuild or
autopkgtest, you don't need to do anything.  But if you have installed either
package from jessie-backports (with Propellor or otherwise), you should install
the other from jessie-backports, too.

-}

module Propellor.Property.Sbuild (
	-- * Creating and updating sbuild schroots
	UseCcache(..),
	built,
	-- * Properties for use inside sbuild schroots
	update,
	osDebianStandard,
	-- * Global sbuild configuration
	-- blockNetwork,
	keypairGenerated,
	keypairInsecurelyGenerated,
	usableBy,
	userConfig,
) where

import Propellor.Base
import Propellor.Types.Core
import Propellor.Types.Info
import Propellor.Property.Debootstrap (extractSuite)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Ccache as Ccache
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.ConfFile as ConfFile
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.File as File
-- import qualified Propellor.Property.Firewall as Firewall
import qualified Propellor.Property.Schroot as Schroot
import qualified Propellor.Property.Reboot as Reboot
import qualified Propellor.Property.Localdir as Localdir
import qualified Propellor.Property.User as User

import Data.List

-- | Whether an sbuild schroot should use ccache during builds
--
-- ccache is generally useful but it breaks building some packages.  This data
-- types allows you to toggle it on and off for particular schroots.
data UseCcache = UseCcache | NoCcache

-- | Build and configure a schroot for use with sbuild
--
-- The second parameter should specify, at a minimum, the operating system for
-- the schroot.  This is usually done using a property like 'osDebian'
built
	:: UseCcache
	-> Props metatypes
	-> RevertableProperty (HasInfo + DebianLike) Linux
built cc ps = case schrootSystem ps of
	Nothing -> emitError
	Just s@(System _ arch) -> case extractSuite s of
		Nothing -> emitError
		Just suite -> built' cc ps suite
			(architectureToDebianArchString arch)
  where
	schrootSystem :: Props metatypes -> Maybe System
	schrootSystem (Props ps') = fromInfoVal . fromInfo $
		mconcat (map getInfo ps')

	emitError :: RevertableProperty (HasInfo + DebianLike) Linux
	emitError = impossible theError <!> impossible theError
	theError = "sbuild schroot does not specify suite and/or architecture"

built'
	:: UseCcache
	-> Props metatypes
	-> String
	-> String
	-> RevertableProperty (HasInfo + DebianLike) Linux
built' cc (Props ps) suite arch = provisioned <!> deleted
  where
	provisioned :: Property (HasInfo + DebianLike)
	provisioned = combineProperties desc $ props
		& cleanupOldConfig
		& overlaysKernel
		& preReqsInstalled
		& ccacheMaybePrepared cc
		& Chroot.provisioned schroot
		& conf suite arch
	  where
		desc = "built sbuild schroot for " ++ suiteArch

	-- TODO we should kill any sessions still using the chroot
	-- before destroying it (as suggested by sbuild-destroychroot)
	deleted :: Property Linux
	deleted = combineProperties desc $ props
		! Chroot.provisioned schroot
		! compatSymlink
		& File.notPresent schrootConf
	  where
		desc = "no sbuild schroot for " ++ suiteArch

	conf suite' arch' = combineProperties "sbuild config file" $ props
		& pair "description" (suite' ++ "/" ++ arch' ++ " autobuilder")
		& pair "groups" "root,sbuild"
		& pair "root-groups" "root,sbuild"
		& pair "profile" "sbuild"
		& pair "type" "directory"
		& pair "directory" schrootRoot
		& unionTypeOverlay
		& aliasesLine
		& pair "command-prefix" (intercalate "," commandPrefix)
	  where
		pair k v = ConfFile.containsIniSetting schrootConf
			(suiteArch ++ "-sbuild", k, v)
		unionTypeOverlay :: Property DebianLike
		unionTypeOverlay = property' "add union-type = overlay" $ \w ->
			Schroot.usesOverlays >>= \usesOverlays ->
				if usesOverlays
				then ensureProperty w $
					pair "union-type" "overlay"
				else noChange

	compatSymlink = File.isSymlinkedTo
		("/etc/sbuild/chroot" </> suiteArch ++ "-sbuild")
		(File.LinkTarget schrootRoot)

	-- if we're building a sid chroot, add useful aliases
	-- In order to avoid more than one schroot getting the same aliases, we
	-- only do this if the arch of the chroot equals the host arch.
	aliasesLine :: Property UnixLike
	aliasesLine = property' "maybe set aliases line" $ \w ->
		sidHostArchSchroot suite arch >>= \isSidHostArchSchroot ->
			if isSidHostArchSchroot
			then ensureProperty w $
				ConfFile.containsIniSetting schrootConf
					( suiteArch ++ "-sbuild"
					, "aliases"
					, aliases
					)
			else return NoChange

	-- if the user has indicated that this host should use
	-- union-type=overlay schroots, we need to ensure that we have rebooted
	-- to a kernel supporting OverlayFS.  Otherwise, executing sbuild(1)
	-- will fail.
	overlaysKernel :: Property DebianLike
	overlaysKernel = property' "reboot for union-type=overlay" $ \w ->
		Schroot.usesOverlays >>= \usesOverlays ->
			if usesOverlays
			then ensureProperty w $
				Reboot.toKernelNewerThan "3.18"
			else noChange

	-- clean up config from earlier versions of this module
	cleanupOldConfig :: Property UnixLike
	cleanupOldConfig =
		property' "old sbuild module config cleaned up" $ \w -> do
			void $ ensureProperty w $
				check (doesFileExist fstab)
				(File.lacksLine fstab aptCacheLine)
			void $ liftIO . tryIO $ removeDirectoryRecursive profile
			void $ liftIO $ nukeFile schrootPiupartsConf
			-- assume this did nothing
			noChange
	  where
		fstab = "/etc/schroot/sbuild/fstab"
		profile = "/etc/schroot/piuparts"
		schrootPiupartsConf = "/etc/schroot/chroot.d"
			</> suiteArch ++ "-piuparts-propellor"

	-- the schroot itself
	schroot = Chroot.debootstrapped Debootstrap.BuilddD
			schrootRoot (Props schrootProps)
	schrootProps =
		ps ++ [toChildProperty $ Apt.installed ["eatmydata", "ccache"]
		-- Drop /usr/local/propellor since build chroots should be
		-- clean.  Note that propellor does not have to install its
		-- build-deps into the chroot, so this is sufficient cleanup
		, toChildProperty $ Localdir.removed]

	-- static values
	suiteArch = suite ++ "-" ++ arch
	schrootRoot = "/srv/chroot" </> suiteArch
	schrootConf = "/etc/schroot/chroot.d"
		</> suiteArch ++ "-sbuild-propellor"
	aliases = intercalate ","
		[ "sid"
		-- if the user wants to build for experimental, they would use
		-- their sid chroot and sbuild's --extra-repository option to
		-- enable experimental
		, "rc-buggy"
		, "experimental"
		-- we assume that building for UNRELEASED means building for
		-- unstable
		, "UNRELEASED"
		-- the following is for dgit compatibility:
		, "UNRELEASED-"
			++ arch
			++ "-sbuild"
		]
	commandPrefix = case cc of
		UseCcache -> "/var/cache/ccache-sbuild/sbuild-setup":base
		_ -> base
	  where
		base = ["eatmydata"]

-- | Properties that will be wanted in almost any Debian schroot, but not in
-- schroots for other operating systems.
osDebianStandard :: Property Debian
osDebianStandard = propertyList "standard Debian sbuild properties" $ props
	& Apt.stdSourcesList

-- | Ensure that an sbuild schroot's packages and apt indexes are updated
--
-- This replaces use of sbuild-update(1).
update :: Property DebianLike
update = Apt.update `before` Apt.upgrade `before` Apt.autoRemove

aptCacheLine :: String
aptCacheLine = "/var/cache/apt/archives /var/cache/apt/archives none rw,bind 0 0"

-- | Ensure that sbuild and associated utilities are installed
preReqsInstalled :: Property DebianLike
preReqsInstalled = Apt.installed ["piuparts", "autopkgtest", "lintian", "sbuild"]

-- | Add an user to the sbuild group in order to use sbuild
usableBy :: User -> Property DebianLike
usableBy u = User.hasGroup u (Group "sbuild") `requires` preReqsInstalled

-- | Generate the apt keys needed by sbuild
--
-- You only need this if you are using sbuild older than 0.70.0.
keypairGenerated :: Property DebianLike
keypairGenerated = check (not <$> doesFileExist secKeyFile) $ go
	`requires` preReqsInstalled
	-- Work around Debian bug #792100 which is present in Jessie.
	-- Since this is a harmless mkdir, don't actually check the OS
	`requires` File.dirExists "/root/.gnupg"
  where
	go :: Property DebianLike
	go = tightenTargets $
		cmdProperty "sbuild-update" ["--keygen"]
		`assume` MadeChange

secKeyFile :: FilePath
secKeyFile = "/var/lib/sbuild/apt-keys/sbuild-key.sec"

-- | Generate the apt keys needed by sbuild using a low-quality source of
-- randomness
--
-- Note that any running rngd will be killed; if you are using rngd, you should
-- arrange for it to be restarted after this property has been ensured.  E.g.
--
-- >  & Sbuild.keypairInsecurelyGenerated
-- >  	`onChange` Systemd.started "my-rngd-service"
--
-- Useful on throwaway build VMs.
--
-- You only need this if you are using sbuild older than 0.70.0.
keypairInsecurelyGenerated :: Property DebianLike
keypairInsecurelyGenerated = check (not <$> doesFileExist secKeyFile) go
  where
	go :: Property DebianLike
	go = combineProperties "sbuild keyring insecurely generated" $ props
		& Apt.installed ["rng-tools"]
		-- If this dir does not exist the sbuild key generation command
		-- will fail; the user might have deleted it to work around
		-- #831462
		& File.dirExists "/var/lib/sbuild/apt-keys"
		-- If there is already an rngd process running we have to kill
		-- it, as it might not be feeding to /dev/urandom.  We can't
		-- kill by pid file because that is not guaranteed to be the
		-- default (/var/run/rngd.pid), so we killall
		& userScriptProperty (User "root")
			[ "start-stop-daemon -q -K -R 10 -o -n rngd"
			, "rngd -r /dev/urandom"
			]
			`assume` MadeChange
		& keypairGenerated
		-- Kill off the rngd process we spawned
		& userScriptProperty (User "root")
			["kill $(cat /var/run/rngd.pid)"]
			`assume` MadeChange

ccacheMaybePrepared :: UseCcache -> Property DebianLike
ccacheMaybePrepared cc = case cc of
	UseCcache -> ccachePrepared
	NoCcache  -> doNothing

-- another script from wiki.d.o/sbuild
ccachePrepared :: Property DebianLike
ccachePrepared = propertyList "sbuild group ccache configured" $ props
	-- We only set a limit on the cache if it doesn't already exist, so the
	-- user can override our default limit
	& check (not <$> doesDirectoryExist "/var/cache/ccache-sbuild")
		(Ccache.hasLimits "/var/cache/ccache-sbuild" (Ccache.MaxSize "2G"))
	`before` Ccache.hasCache (Group "sbuild") Ccache.NoLimit
	& "/etc/schroot/sbuild/fstab" `File.containsLine`
	"/var/cache/ccache-sbuild /var/cache/ccache-sbuild none rw,bind 0 0"
		`describe` "ccache mounted in sbuild schroots"
	& "/var/cache/ccache-sbuild/sbuild-setup" `File.hasContent`
		[ "#!/bin/sh"
		, ""
		, "export CCACHE_DIR=/var/cache/ccache-sbuild"
		, "export CCACHE_UMASK=002"
		, "export CCACHE_COMPRESS=1"
		, "unset CCACHE_HARDLINK"
		, "export PATH=\"/usr/lib/ccache:$PATH\""
		, ""
		, "exec \"$@\""
		]
	& File.mode "/var/cache/ccache-sbuild/sbuild-setup"
		(combineModes (readModes ++ executeModes))

-- This doesn't seem to work with the current version of sbuild
-- -- | Block network access during builds
-- --
-- -- This is a hack from <https://wiki.debian.org/sbuild> until #802850 and
-- -- #802849 are resolved.
-- blockNetwork :: Property Linux
-- blockNetwork = Firewall.rule Firewall.OUTPUT Firewall.Filter Firewall.DROP
-- 	(Firewall.GroupOwner (Group "sbuild")
-- 	<> Firewall.NotDestination
-- 		[Firewall.IPWithNumMask (IPv4 "127.0.0.1") 8])
-- 	`requires` installed 	-- sbuild group must exist

-- | Maintain recommended ~/.sbuildrc for a user, and adds them to the
-- sbuild group
--
-- You probably want a custom ~/.sbuildrc on your workstation, but
-- this property is handy for quickly setting up build boxes.
--
-- On Debian jessie hosts, you should ensure that sbuild and autopkgtest come
-- from the same suite.  This is because the autopkgtest binary changed its name
-- between jessie and stretch.  If you have not installed backports of sbuild or
-- autopkgtest, you don't need to do anything.  But if you have installed either
-- package from jessie-backports (with Propellor or otherwise), you should
-- install the other from jessie-backports, too.
userConfig :: User -> Property DebianLike
userConfig user@(User u) = go
	`requires` usableBy user
	`requires` preReqsInstalled
  where
	go :: Property DebianLike
	go = property' ("~/.sbuildrc for " ++ u) $ \w -> do
		h <- liftIO (User.homedir user)
		ensureProperty w $ File.hasContent (h </> ".sbuildrc")
			[ "$run_lintian = 1;"
			, ""
			, "$run_piuparts = 1;"
			, "$piuparts_opts = ["
			, "    '--no-eatmydata',"
			, "    '--schroot',"
			, "    '%r-%a-sbuild',"
			, "    '--fail-if-inadequate',"
			, "    ];"
			, ""
			, "$run_autopkgtest = 1;"
			, "$autopkgtest_root_args = \"\";"
			, "$autopkgtest_opts = [\"--\", \"schroot\", \"%r-%a-sbuild\"];"
			]

-- ==== utility functions ====

-- Determine whether a schroot is
--
-- (i)  Debian sid, and
-- (ii) the same architecture as the host.
--
-- This is the "sid host arch schroot".  It is considered the default schroot
-- for sbuild builds, so we add useful aliases that work well with the suggested
-- ~/.sbuildrc given in the haddock
sidHostArchSchroot :: String -> String -> Propellor Bool
sidHostArchSchroot suite arch = do
	maybeOS <- getOS
	return $ case maybeOS of
		Nothing -> False
		Just (System _ hostArch) ->
			let hostArch' = architectureToDebianArchString hostArch
			in suite == "unstable" && hostArch' == arch
