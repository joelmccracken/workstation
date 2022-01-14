module Propellor.Property.OS (
	cleanInstallOnce,
	Confirmation(..),
	preserveNetwork,
	preserveResolvConf,
	preserveRootSshAuthorized,
	oldOSRemoved,
) where

import Propellor.Base
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Network as Network
import qualified Propellor.Property.User as User
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Reboot as Reboot
import Propellor.Property.Mount
import Propellor.Property.Chroot.Util (stdPATH)

import System.Posix.Files (rename, fileExist)
import Control.Exception (throw)

-- | Replaces whatever OS was installed before with a clean installation
-- of the OS that the Host is configured to have.
--
-- This is experimental; use with caution!
--
-- This can replace one Linux distribution with different one.
-- But, it can also fail and leave the system in an unbootable state.
--
-- To avoid this property being accidentially used, you have to provide
-- a Confirmation containing the name of the host that you intend to apply
-- the property to.
--
-- This property only runs once. The cleanly installed system will have
-- a file </etc/propellor-cleaninstall>, which indicates it was cleanly
-- installed.
--
-- The files from the old os will be left in </old-os>
--
-- After the OS is installed, and if all properties of the host have
-- been successfully satisfied, the host will be rebooted to properly load
-- the new OS.
--
-- You will typically want to run some more properties after the clean
-- install succeeds, to bootstrap from the cleanly installed system to
-- a fully working system. For example:
--
-- > & osDebian Unstable X86_64
-- > & cleanInstallOnce (Confirmed "foo.example.com")
-- >    `onChange` propertyList "fixing up after clean install"
-- >        [ preserveNetwork
-- >        , preserveResolvConf
-- >        , preserveRootSshAuthorized
-- >        , Apt.update
-- >        -- , Grub.boots "/dev/sda"
-- >        --   `requires` Grub.installed Grub.PC
-- >        -- , oldOsRemoved (Confirmed "foo.example.com")
-- >        ]
-- > & Hostname.sane
-- > & Hostname.mailname
-- > & Apt.installed ["linux-image-amd64"]
-- > & Apt.installed ["ssh"]
-- > & User.hasSomePassword "root"
-- > & User.accountFor "joey"
-- > & User.hasSomePassword "joey"
-- > -- rest of system properties here
cleanInstallOnce :: Confirmation -> Property DebianLike
cleanInstallOnce confirmation = check (not <$> doesFileExist flagfile) $
	go `requires` confirmed "clean install confirmed" confirmation
  where
	go =
		finalized
			`requires`
		-- easy to forget and system may not boot without shadow pw!
		User.shadowConfig True
			`requires`
		-- reboot at end if the rest of the propellor run succeeds
		Reboot.atEnd True (/= FailedChange)
			`requires`
		propellorbootstrapped
			`requires`
		flipped
			`requires`
		osbootstrapped

	osbootstrapped :: Property Linux
	osbootstrapped = withOS (newOSDir ++ " bootstrapped") $ \w o -> case o of
		(Just d@(System (Debian _ _) _)) -> ensureProperty w $
			debootstrap d
		(Just u@(System (Buntish _) _)) -> ensureProperty w $
			debootstrap u
		_ -> unsupportedOS'

	debootstrap :: System -> Property Linux
	debootstrap targetos =
		-- Install debootstrap from source, since we don't know
		-- what OS we're currently running in.
		Debootstrap.built' Debootstrap.sourceInstall
			newOSDir targetos Debootstrap.DefaultConfig
		-- debootstrap, I wish it was faster..
		-- TODO eatmydata to speed it up
		-- Problem: Installing eatmydata on some random OS like
		-- Fedora may be difficult. Maybe configure dpkg to not
		-- sync instead?

	-- This is the fun bit.
	flipped :: Property Linux
	flipped = property (newOSDir ++ " moved into place") $ liftIO $ do
		-- First, unmount most mount points, lazily, so
		-- they don't interfere with moving things around.
		devfstype <- fromMaybe "devtmpfs" <$> getFsType "/dev"
		mnts <- filter (`notElem` ("/": trickydirs)) <$> mountPoints
		-- reverse so that deeper mount points come first
		forM_ (reverse mnts) umountLazy

		renamesout <- map (\d -> (d, oldOSDir ++ d, pure $ d `notElem` (oldOSDir:newOSDir:trickydirs)))
			<$> dirContents "/"
		renamesin <- map (\d -> let dest = "/" ++ takeFileName d in (d, dest, not <$> fileExist dest))
			<$> dirContents newOSDir
		createDirectoryIfMissing True oldOSDir
		massRename (renamesout ++ renamesin)
		removeDirectoryRecursive newOSDir

		-- Prepare environment for running additional properties,
		-- overriding old OS's environment.
		void $ setEnv "PATH" stdPATH True
		void $ unsetEnv "LANG"

		-- Remount /dev, so that block devices etc are
		-- available for other properties to use.
		unlessM (mount devfstype devfstype "/dev" mempty) $ do
			warningMessage $ "failed mounting /dev using " ++ devfstype ++ "; falling back to MAKEDEV generic"
			void $ boolSystem "sh" [Param "-c", Param "cd /dev && /sbin/MAKEDEV generic"]

		-- Mount /sys too, needed by eg, grub-mkconfig.
		unlessM (mount "sysfs" "sysfs" "/sys" mempty) $
			warningMessage "failed mounting /sys"

		-- And /dev/pts, used by apt.
		unlessM (mount "devpts" "devpts" "/dev/pts" mempty) $
			warningMessage "failed mounting /dev/pts"

		return MadeChange

	propellorbootstrapped :: Property UnixLike
	propellorbootstrapped = property "propellor re-debootstrapped in new os" $
		return NoChange
		-- re-bootstrap propellor in /usr/local/propellor,
		--   (using git repo bundle, privdata file, and possibly
		--   git repo url, which all need to be arranged to
		--   be present in /old-os's /usr/local/propellor)
		-- TODO

	finalized :: Property UnixLike
	finalized = property "clean OS installed" $ do
		liftIO $ writeFile flagfile ""
		return MadeChange

	flagfile = "/etc/propellor-cleaninstall"

	trickydirs =
		-- /tmp can contain X's sockets, which prevent moving it
		-- so it's left as-is.
		[ "/tmp"
		-- /proc is left mounted
		, "/proc"
		]

-- Performs all the renames. If any rename fails, rolls back all
-- previous renames. Thus, this either successfully performs all
-- the renames, or does not change the system state at all.
massRename :: [(FilePath, FilePath, IO Bool)] -> IO ()
massRename = go []
  where
	go _ [] = return ()
	go undo ((from, to, test):rest) = ifM test
		( tryNonAsync (rename from to)
			>>= either
				(rollback undo)
				(const $ go ((to, from):undo) rest)
		, go undo rest
		)
	rollback undo e = do
		mapM_ (uncurry rename) undo
		throw e

data Confirmation = Confirmed HostName

confirmed :: Desc -> Confirmation -> Property UnixLike
confirmed desc (Confirmed c) = property desc $ do
	hostname <- asks hostName
	if hostname /= c
		then do
			warningMessage "Run with a bad confirmation, not matching hostname."
			return FailedChange
		else return NoChange

-- | </etc/network/interfaces> is configured to bring up the network
-- interface that currently has a default route configured, using
-- the same (static) IP address.
preserveNetwork :: Property DebianLike
preserveNetwork = go `requires` Network.cleanInterfacesFile
  where
	go :: Property DebianLike
	go = property' "preserve network configuration" $ \w -> do
		ls <- liftIO $ lines <$> readProcess "ip"
			["route", "list", "scope", "global"]
		case words <$> headMaybe ls of
			Just ("default":"via":_:"dev":iface:_) ->
				ensureProperty w $ Network.preserveStatic iface
			_ -> do
				warningMessage "did not find any default ipv4 route"
				return FailedChange

-- | </etc/resolv.conf> is copied from the old OS
preserveResolvConf :: Property Linux
preserveResolvConf = check (fileExist oldloc) $
	property' (newloc ++ " copied from old OS") $ \w -> do
		ls <- liftIO $ lines <$> readFile oldloc
		ensureProperty w $ newloc `File.hasContent` ls
  where
	newloc = "/etc/resolv.conf"
	oldloc = oldOSDir ++ newloc

-- | </root/.ssh/authorized_keys> has added to it any ssh keys that
-- were authorized in the old OS. Any other contents of the file are
-- retained.
preserveRootSshAuthorized :: Property UnixLike
preserveRootSshAuthorized = check (fileExist oldloc) $
	property' desc $ \w -> do
		ks <- liftIO $ lines <$> readFile oldloc
		ensureProperty w $ combineProperties desc $
			toProps $ map (setupRevertableProperty . Ssh.authorizedKey (User "root")) ks
  where
	desc = newloc ++ " copied from old OS"
	newloc = "/root/.ssh/authorized_keys"
	oldloc = oldOSDir ++ newloc

-- Removes the old OS's backup from </old-os>
oldOSRemoved :: Confirmation -> Property UnixLike
oldOSRemoved confirmation = check (doesDirectoryExist oldOSDir) $
	go `requires` confirmed "old OS backup removal confirmed" confirmation
  where
	go :: Property UnixLike
	go = property "old OS backup removed" $ do
		liftIO $ removeDirectoryRecursive oldOSDir
		return MadeChange

oldOSDir :: FilePath
oldOSDir = "/old-os"

newOSDir :: FilePath
newOSDir = "/new-os"
