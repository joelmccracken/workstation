{-# LANGUAGE FlexibleContexts, GADTs, DeriveDataTypeable #-}

module Propellor.Property.Chroot (
	debootstrapped,
	bootstrapped,
	provisioned,
	hostChroot,
	Chroot(..),
	ChrootBootstrapper(..),
	Debootstrapped(..),
	ChrootTarball(..),
	exposeTrueLocaldir,
	useHostProxy,
	-- * Internal use
	provisioned',
	propagateChrootInfo,
	propellChroot,
	chain,
	chrootSystem,
) where

import Propellor.Base
import Propellor.Container
import Propellor.Types.CmdLine
import Propellor.Types.Chroot
import Propellor.Types.Container
import Propellor.Types.Info
import Propellor.Types.Core
import Propellor.Property.Chroot.Util
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.Systemd.Core as Systemd
import qualified Propellor.Property.File as File
import qualified Propellor.Shim as Shim
import Propellor.Property.Mount
import Utility.Split

import qualified Data.Map as M
import System.Posix.Directory

-- | Specification of a chroot. Normally you'll use `debootstrapped` or
-- `bootstrapped` or `hostChroot` to construct a Chroot value.
data Chroot where
	Chroot :: ChrootBootstrapper b => FilePath -> b -> InfoPropagator -> Host -> Chroot

instance IsContainer Chroot where
	containerProperties (Chroot _ _ _ h) = containerProperties h
	containerInfo (Chroot _ _ _ h) = containerInfo h
	setContainerProperties (Chroot loc b p h) ps =
		let h' = setContainerProperties h ps
		in Chroot loc b p h'

chrootSystem :: Chroot -> Maybe System
chrootSystem = fromInfoVal . fromInfo . containerInfo

instance Show Chroot where
	show c@(Chroot loc _ _ _) = "Chroot " ++ loc ++ " " ++ show (chrootSystem c)

-- | Class of things that can do initial bootstrapping of an operating
-- System in a chroot.
class ChrootBootstrapper b where
	-- | Do initial bootstrapping of an operating system in a chroot.
	-- If the operating System is not supported, return
	-- Left error message.
	buildchroot 
		:: b
		-> Info -- ^ info of the Properties of the chroot
		-> FilePath -- ^ where to bootstrap the chroot
		-> Either String (Property Linux)

-- | Use this to bootstrap a chroot by extracting a tarball.
--
-- The tarball is expected to contain a root directory (no top-level
-- directory, also known as a "tarbomb").
-- It may be optionally compressed with any format `tar` knows how to
-- detect automatically.
data ChrootTarball = ChrootTarball FilePath

instance ChrootBootstrapper ChrootTarball where
	buildchroot (ChrootTarball tb) _ loc = Right $
		tightenTargets $ extractTarball loc tb

extractTarball :: FilePath -> FilePath -> Property UnixLike
extractTarball target src = check (isUnpopulated target) $
	cmdProperty "tar" params
		`assume` MadeChange
		`requires` File.dirExists target
  where
	params =
		[ "-C"
		, target
		, "-xf"
		, src
		]

-- | Use this to bootstrap a chroot with debootstrap.
data Debootstrapped = Debootstrapped Debootstrap.DebootstrapConfig

instance ChrootBootstrapper Debootstrapped where
	buildchroot (Debootstrapped cf) info loc = case system of
		(Just s@(System (Debian _ _) _)) -> Right $ debootstrap s
		(Just s@(System (Buntish _) _)) -> Right $ debootstrap s
		(Just (System ArchLinux _)) -> Left "Arch Linux not supported by debootstrap."
		(Just (System (FreeBSD _) _)) -> Left "FreeBSD not supported by debootstrap."
		Nothing -> Left "Cannot debootstrap; OS not specified"
	  where
		debootstrap s = Debootstrap.built loc s
			(cf <> proxyConf <> mirrorConf)
		system = fromInfoVal (fromInfo info)
		-- If the chroot has a configured apt proxy and/or mirror, pass
		-- these on to debootstrap.  Note that Debootstrap.built does
		-- not get passed the Chroot, so the info inspection has to
		-- happen here, not there
		proxyConf = case (fromInfoVal . fromInfo) info of
			Just (Apt.HostAptProxy u) ->
				Debootstrap.DebootstrapProxy u
			Nothing -> mempty
		mirrorConf = case (fromInfoVal . fromInfo) info of
			Just (Apt.HostMirror u) ->
				Debootstrap.DebootstrapMirror u
			Nothing -> mempty

-- | Defines a Chroot at the given location, built with debootstrap.
--
-- Properties can be added to configure the Chroot. At a minimum,
-- add a property such as `osDebian` to specify the operating system
-- to bootstrap.
--
-- If the 'Debootstrap.DebootstrapConfig' does not include a 
-- 'Debootstrap.DebootstrapMirror',
-- any 'Apt.mirror' property of the chroot will configure debootstrap.
-- Same for 'Debootstrap.DebootstrapProxy' and 'Apt.proxy'.
--
-- > debootstrapped Debootstrap.BuildD "/srv/chroot/ghc-dev" $ props
-- >	& osDebian Unstable X86_64
-- >	& Apt.installed ["ghc", "haskell-platform"]
-- >	& ...
debootstrapped :: Debootstrap.DebootstrapConfig -> FilePath -> Props metatypes -> Chroot
debootstrapped conf = bootstrapped (Debootstrapped conf)

-- | Defines a Chroot at the given location, bootstrapped with the
-- specified ChrootBootstrapper.
--
-- Like 'Chroot.debootstrapped', if the 'ChrootBootstrapper' is
-- 'Debootstrapped', this property respects the Chroot's
-- 'Apt.proxy' and 'Apt.mirror' properties.
bootstrapped :: ChrootBootstrapper b => b -> FilePath -> Props metatypes -> Chroot
bootstrapped bootstrapper location ps = c
  where
	c = Chroot location bootstrapper propagateChrootInfo (host location ps)

-- | Ensures that the chroot exists and is provisioned according to its
-- properties.
--
-- Reverting this property removes the chroot. Anything mounted inside it
-- is first unmounted. Note that it does not ensure that any processes
-- that might be running inside the chroot are stopped.
provisioned :: Chroot -> RevertableProperty (HasInfo + Linux) Linux
provisioned c = provisioned' c False [FilesystemContained]

provisioned'
	:: Chroot
	-> Bool
	-> [ContainerCapability]
	-> RevertableProperty (HasInfo + Linux) Linux
provisioned' c@(Chroot loc bootstrapper infopropigator _) systemdonly caps =
	(infopropigator c normalContainerInfo $ setup `describe` chrootDesc c "exists")
		<!>
	(teardown `describe` chrootDesc c "removed")
  where
	setup :: Property Linux
	setup = propellChroot c (inChrootProcess (not systemdonly) c) systemdonly caps
		`requires` built

	built = case buildchroot bootstrapper (containerInfo c) loc of
		Right p -> p
		Left e -> cantbuild e

	cantbuild e = property (chrootDesc c "built") (error e)

	teardown :: Property Linux
	teardown = check (not <$> isUnpopulated loc) $
		property ("removed " ++ loc) $
			makeChange (removeChroot loc)

type InfoPropagator = Chroot -> (PropagateInfo -> Bool) -> Property Linux -> Property (HasInfo + Linux)

propagateChrootInfo :: InfoPropagator
propagateChrootInfo c@(Chroot location _ _ _) pinfo p =
	propagateContainer location c pinfo $
		p `setInfoProperty` chrootInfo c

chrootInfo :: Chroot -> Info
chrootInfo (Chroot loc _ _ h) = mempty `addInfo`
	mempty { _chroots = M.singleton loc h }

-- | Propellor is run inside the chroot to provision it.
propellChroot :: Chroot -> ([String] -> IO (CreateProcess, IO ())) -> Bool -> [ContainerCapability] -> Property UnixLike
propellChroot c@(Chroot loc _ _ _) mkproc systemdonly caps = property (chrootDesc c "provisioned") $ do
	let d = localdir </> shimdir c
	let me = localdir </> "propellor"
	shim <- liftIO $ Shim.setup me Nothing d
	ifM (liftIO $ bindmount shim)
		( chainprovision shim
		, return FailedChange
		)
  where
	bindmount shim = ifM (doesFileExist (loc ++ shim))
		( return True
		, do
			let mntpnt = loc ++ localdir
			createDirectoryIfMissing True mntpnt
			boolSystem "mount"
				[ Param "--bind"
				, File localdir, File mntpnt
				]
		)

	chainprovision shim = do
		parenthost <- asks hostName
		cmd <- liftIO $ toChain parenthost c systemdonly caps
		pe <- liftIO standardPathEnv
		(p, cleanup) <- liftIO $ mkproc
			[ shim
			, "--continue"
			, show cmd
			]
		r <- liftIO $ chainPropellor (p { env = Just pe })
		liftIO cleanup
		return r

toChain :: HostName -> Chroot -> Bool -> [ContainerCapability] -> IO CmdLine
toChain parenthost (Chroot loc _ _ _) systemdonly caps = do
	onconsole <- isConsole <$> getMessageHandle
	return $ ChrootChain parenthost loc systemdonly onconsole caps

chain :: [Host] -> CmdLine -> IO ()
chain hostlist (ChrootChain hn loc systemdonly onconsole caps) =
	case findHostNoAlias hostlist hn of
		Nothing -> errorMessage ("cannot find host " ++ hn)
		Just parenthost -> case M.lookup loc (_chroots $ fromInfo $ hostInfo parenthost) of
			Nothing -> errorMessage ("cannot find chroot " ++ loc ++ " on host " ++ hn)
			Just h -> go h
  where
	go h = do
		changeWorkingDirectory localdir
		when onconsole forceConsole
		onlyProcess (provisioningLock loc) $
			runChainPropellor (setcaps h) $
				ensureChildProperties $
					if systemdonly
						then [toChildProperty Systemd.installed]
						else hostProperties h
	setcaps h = h { hostInfo = hostInfo h `addInfo` caps }
chain _ _ = errorMessage "bad chain command"

inChrootProcess :: Bool -> Chroot -> [String] -> IO (CreateProcess, IO ())
inChrootProcess keepprocmounted (Chroot loc _ _ _) cmd = do
	mountproc
	return (proc "chroot" (loc:cmd), cleanup)
  where
	-- /proc needs to be mounted in the chroot for the linker to use
	-- /proc/self/exe which is necessary for some commands to work
	mountproc = unlessM (elem procloc <$> mountPointsBelow loc) $
		void $ mount "proc" "proc" procloc mempty

	procloc = loc </> "proc"

	cleanup
		| keepprocmounted = noop
		| otherwise = whenM (elem procloc <$> mountPointsBelow loc) $
			umountLazy procloc

provisioningLock :: FilePath -> FilePath
provisioningLock containerloc = "chroot" </> mungeloc containerloc ++ ".lock"

shimdir :: Chroot -> FilePath
shimdir (Chroot loc _ _ _) = "chroot" </> mungeloc loc ++ ".shim"

mungeloc :: FilePath -> String
mungeloc = replace "/" "_"

chrootDesc :: Chroot -> String -> String
chrootDesc (Chroot loc _ _ _) desc = "chroot " ++ loc ++ " " ++ desc

-- | Runs an action with the true localdir exposed,
-- not the one bind-mounted into a chroot. The action is passed the
-- path containing the contents of the localdir outside the chroot.
--
-- In a chroot, this is accomplished by temporily bind mounting the localdir
-- to a temp directory, to preserve access to the original bind mount. Then
-- we unmount the localdir to expose the true localdir. Finally, to cleanup,
-- the temp directory is bind mounted back to the localdir.
exposeTrueLocaldir :: (FilePath -> Propellor a) -> Propellor a
exposeTrueLocaldir a = ifM (hasContainerCapability FilesystemContained)
	( withTmpDirIn (takeDirectory localdir) "propellor.tmp" $ \tmpdir ->
		bracket_
			(movebindmount localdir tmpdir)
			(movebindmount tmpdir localdir)
			(a tmpdir)
	, a localdir
	)
  where
	movebindmount from to = liftIO $ do
		run "mount" [Param "--bind", File from, File to]
		-- Have to lazy unmount, because the propellor process
		-- is running in the localdir that it's unmounting..
		run "umount" [Param "-l", File from]
		-- We were in the old localdir; move to the new one after
		-- flipping the bind mounts. Otherwise, commands that try
		-- to access the cwd will fail because it got umounted out
		-- from under.
		changeWorkingDirectory "/"
		changeWorkingDirectory localdir
	run cmd ps = unlessM (boolSystem cmd ps) $
		error $ "exposeTrueLocaldir failed to run " ++ show (cmd, ps)

-- | Generates a Chroot that has all the properties of a Host.
-- 
-- Note that it's possible to create loops using this, where a host
-- contains a Chroot containing itself etc. Such loops will be detected at
-- runtime.
hostChroot :: ChrootBootstrapper bootstrapper => Host -> bootstrapper -> FilePath -> Chroot
hostChroot h bootstrapper d = chroot
  where
	chroot = Chroot d bootstrapper pinfo h
	pinfo = propagateHostChrootInfo h

-- This is different than propagateChrootInfo in that Info using
-- HostContext is not made to use the name of the chroot as its context,
-- but instead uses the hostname of the Host.
propagateHostChrootInfo :: Host -> InfoPropagator
propagateHostChrootInfo h c pinfo p =
	propagateContainer (hostName h) c pinfo $
		p `setInfoProperty` chrootInfo c

-- | Ensure that a chroot uses the host's Apt proxy.
--
-- This property is often used for 'Sbuild.built' chroots, when the host has
-- 'Apt.useLocalCacher'.
useHostProxy :: Host -> Property DebianLike
useHostProxy h = property' "use host's apt proxy" $ \w ->
	-- Note that we can't look at getProxyInfo outside the property,
	-- as that would loop, but it's ok to look at it inside the
	-- property. Thus the slightly strange construction here.
	case getProxyInfo h of
		Just (Apt.HostAptProxy u) -> ensureProperty w (Apt.proxy' u)
		Nothing -> noChange
  where
	getProxyInfo = fromInfoVal . fromInfo . hostInfo
