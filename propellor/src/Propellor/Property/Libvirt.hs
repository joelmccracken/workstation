-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Libvirt (
	NumVCPUs(..),
	MiBMemory(..),
	AutoStart(..),
	DiskImageType(..),
	installed,
	defaultNetworkAutostarted,
	defaultNetworkStarted,
	defined,
) where

import Propellor.Base
import Propellor.Types.Info
import Propellor.Property.Chroot
import Propellor.Property.DiskImage
import qualified Propellor.Property.Apt as Apt

import Utility.Split

-- | The number of virtual CPUs to assign to the virtual machine
newtype NumVCPUs = NumVCPUs Int

-- | The number of MiB of memory to assign to the virtual machine
newtype MiBMemory = MiBMemory Int

-- | Whether the virtual machine should be started after it is defined, and at
-- host system boot
data AutoStart = AutoStart | NoAutoStart

-- | Which type of disk image to build for the virtual machine
data DiskImageType = Raw -- TODO: | QCow2

-- | Install basic libvirt components
installed :: Property DebianLike
installed = Apt.installed ["libvirt-clients", "virtinst", "libvirt-daemon", "libvirt-daemon-system"]

-- | Ensure that the default libvirt network is set to autostart, and start it.
--
-- On Debian, it is not started by default after installation of libvirt.
defaultNetworkAutostarted :: Property DebianLike
defaultNetworkAutostarted = autostarted
	`requires` installed
	`before` defaultNetworkStarted
  where
	autostarted = check (not <$> doesFileExist autostartFile) $
		cmdProperty "virsh" ["net-autostart", "default"]
	autostartFile = "/etc/libvirt/qemu/networks/autostart/default.xml"

-- | Ensure that the default libvirt network is started.
defaultNetworkStarted :: Property DebianLike
defaultNetworkStarted =	go `requires` installed
  where
	go :: Property UnixLike
	go = property "start libvirt's default network" $ do
		runningNetworks <- liftIO $ virshGetColumns ["net-list"]
		if ["default"] `elem` (take 1 <$> runningNetworks)
			then noChange
			else makeChange $ unlessM startIt $
				errorMessage "failed to start default network"
	startIt = boolSystem "virsh" [Param "net-start", Param "default"]


-- | Builds a disk image with the properties of the given Host, installs a
-- libvirt configuration file to boot the image, and if it is set to autostart,
-- start the VM.
--
-- Note that building the disk image happens only once.  So if you change the
-- properties of the given Host, this property will not modify the disk image.
-- In order to later apply properties to the VM, you should spin it directly, or
-- arrange to have it spun with a property like 'Cron.runPropellor', or use
-- 'Propellor.Property.Conductor' from the VM host.
--
-- Suggested usage in @config.hs@:
--
-- > mybox = host "mybox.example.com" $ props
-- > 	& osDebian (Stable "stretch") X86_64
-- > 	& Libvirt.defaultNetworkAutostarted
-- > 	& Libvirt.defined Libvirt.Raw
-- > 		(Libvirt.MiBMemory 2048) (Libvirt.NumVCPUs 2)
-- > 		Libvirt.NoAutoStart subbox
-- >
-- > subbox = host "subbox.mybox.example.com" $ props
-- > 	& osDebian Unstable X86_64
-- > 	& hasPartition
-- > 		( partition EXT4
-- > 			`mountedAt` "/"
-- > 			`addFreeSpace` MegaBytes 10240
-- > 		)
-- > 	& Apt.installed ["linux-image-amd64"]
-- > 	& Grub.installed PC
-- >
-- > 	& ipv4 "192.168.122.31"
-- > 	& Network.static "ens3" (IPv4 "192.168.122.31")
-- > 		(Just (Network.Gateway (IPv4 "192.168.122.1")))
-- > 		`requires` Network.cleanInterfacesFile
-- > 	& Hostname.sane
defined
	:: DiskImageType
	-> MiBMemory
	-> NumVCPUs
	-> AutoStart
	-> Host
	-> Property (HasInfo + DebianLike)
defined imageType (MiBMemory mem) (NumVCPUs cpus) auto h =
	(built `before` nuked `before` xmlDefined `before` started)
	`requires` installed
  where
	built :: Property (HasInfo + DebianLike)
	built = check (not <$> doesFileExist imageLoc) $
		setupRevertableProperty $ imageBuiltFor h
			(image) (Debootstrapped mempty)

	nuked :: Property UnixLike
	nuked = imageChrootNotPresent image

	xmlDefined :: Property UnixLike
	xmlDefined = check (not <$> doesFileExist conf) $
		property "define the libvirt VM" $
		withTmpFile (hostName h) $ \t fh -> do
			xml <- liftIO $ readProcess "virt-install" $
				[ "-n", hostName h
				, "--memory=" ++ show mem
				, "--vcpus=" ++ show cpus
				, "--disk"
				, "path=" ++ imageLoc
					++ ",device=disk,bus=virtio"
				, "--print-xml"
				] ++ autoStartArg ++ osVariantArg
			liftIO $ hPutStrLn fh xml
			liftIO $ hClose fh
			makeChange $ unlessM (defineIt t) $
				errorMessage "failed to define VM"
	  where
		defineIt t = boolSystem "virsh" [Param "define", Param t]

	started :: Property UnixLike
	started = case auto of
		AutoStart -> property "start the VM" $ do
			runningVMs <- liftIO $ virshGetColumns ["list"]
			-- From the point of view of `virsh start`, the "State"
			-- column in the output of `virsh list` is not relevant.
			-- So long as the VM is listed, it's considered started.
			if [hostName h] `elem` (take 1 . drop 1 <$> runningVMs)
				then noChange
				else makeChange $ unlessM startIt $
					errorMessage "failed to start VM"
		NoAutoStart -> doNothing
	  where
		startIt = boolSystem "virsh" [Param "start", Param $ hostName h]

	image = case imageType of
		Raw -> RawDiskImage imageLoc
	imageLoc =
		"/var/lib/libvirt/images" </> hostName h <.> case imageType of
			Raw -> "img"
	conf = "/etc/libvirt/qemu" </> hostName h <.> "xml"

	osVariantArg = maybe [] (\v -> ["--os-variant=" ++ v]) $ osVariant h
	autoStartArg = case auto of
		AutoStart -> ["--autostart"]
		NoAutoStart -> []

-- ==== utility functions ====

-- The --os-variant property is optional, per virt-install(1), so return Nothing
-- if there isn't a known correct value.  The VM will still be defined.  Pass
-- the value if we can, though, to optimise the generated XML for the host's OS
osVariant :: Host -> Maybe String
osVariant h = hostSystem h >>= \s -> case s of
	System (Debian _ (Stable "jessie")) _ -> Just "debian8"
	System (Debian _ (Stable "stretch")) _ -> Just "debian9"
	System (Debian _ Testing) _ -> Just "debiantesting"
	System (Debian _ Unstable) _ -> Just "debiantesting"

	System (Buntish "trusty") _ -> Just "ubuntu14.04"
	System (Buntish "utopic") _ -> Just "ubuntu14.10"
	System (Buntish "vivid") _ -> Just "ubuntu15.04"
	System (Buntish "wily") _ -> Just "ubuntu15.10"
	System (Buntish "xenial") _ -> Just "ubuntu16.04"
	System (Buntish "yakkety") _ -> Just "ubuntu16.10"
	System (Buntish "zesty") _ -> Just "ubuntu17.04"
	System (Buntish "artful") _ -> Just "ubuntu17.10"
	System (Buntish "bionic") _ -> Just "ubuntu18.04"

	System (FreeBSD (FBSDProduction FBSD101)) _ -> Just "freebsd10.1"
	System (FreeBSD (FBSDProduction FBSD102)) _ -> Just "freebsd10.2"
	System (FreeBSD (FBSDProduction FBSD093)) _ -> Just "freebsd9.3"
	System (FreeBSD (FBSDLegacy FBSD101)) _ -> Just "freebsd10.1"
	System (FreeBSD (FBSDLegacy FBSD102)) _ -> Just "freebsd10.2"
	System (FreeBSD (FBSDLegacy FBSD093)) _ -> Just "freebsd9.3"

	-- libvirt doesn't have an archlinux variant yet, it seems
	System ArchLinux _ -> Nothing

	-- other stable releases that we don't know about (since there are
	-- infinitely many possible stable release names, as it is a freeform
	-- string, we need this to avoid a compiler warning)
	System (Debian _ _) _ -> Nothing
	System (Buntish _) _ -> Nothing

-- Run a virsh command with the given list of arguments, that is expected to
-- yield tabular output, and return the rows
virshGetColumns :: [String] -> IO [[String]]
virshGetColumns args = map (filter (not . null) . split " ") . drop 2 . lines
 	<$> readProcess "virsh" args

hostSystem :: Host -> Maybe System
hostSystem = fromInfoVal . fromInfo . hostInfo
