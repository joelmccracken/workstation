{-# LANGUAGE TypeOperators, RankNTypes, TypeFamilies, FlexibleContexts #-}

-- | Installation to a target disk.
-- 
-- Note that the RevertableProperties in this module are not really
-- revertable; the target disk can't be put back how it was. 
-- The RevertableProperty type is used only to let them  be used
-- in a Versioned Host as shown below.
--
-- Here's an example of a noninteractive installer image using
-- these properties.
--
-- There are two versions of Hosts, the installer and the target system.
-- 
-- > data Variety = Installer | Target
-- > 	deriving (Eq)
-- 
-- The seed of both the installer and the target. They have some properties
-- in common, and some different properties. The `targetInstalled`
-- property knows how to convert the installer it's running on into a
-- target system.
--
-- > seed :: Versioned Variety Host
-- > seed ver = host "debian.local" $ props
-- > 	& osDebian Unstable X86_64
-- > 	& Hostname.sane
-- >	& Hostname.mailname
-- > 	& Apt.stdSourcesList
-- > 	& Apt.installed ["linux-image-amd64"]
-- > 	& Grub.installed PC
-- > 	& "en_US.UTF-8" `Locale.selectedFor` ["LANG"]
-- > 	& ver ( (== Installer) --> targetInstalled seed Target (userInput ver) parts)
-- > 	& ver ( (== Target)    --> fstabLists (userInput ver) parts)
-- > 	& ver ( (== Installer) --> targetBootable (userInput ver))
-- >   where
-- > 	parts = TargetPartTable MSDOS
-- > 		[ partition EXT4 `mountedAt` "/"
-- > 			`useDiskSpace` RemainingSpace
-- > 		, swapPartition (MegaBytes 1024)
-- > 		]
-- 
-- The installer disk image can then be built from the seed as follows:
-- 
-- > installerBuilt :: RevertableProperty (HasInfo + DebianLike) Linux
-- > installerBuilt = imageBuilt (VirtualBoxPointer "/srv/installer.vmdk")
-- >	(hostChroot (seed `version` installer) (Debootstrapped mempty))
-- >	MSDOS
-- > 	 [ partition EXT4 `mountedAt` "/"
-- >		`setFlag` BootFlag
-- >		`reservedSpacePercentage` 0
-- > 		`addFreeSpace` MegaBytes 256
-- > 	]
--
-- When the installer is booted up, and propellor is run, it installs
-- to the target disk. Since this example is a noninteractive installer,
-- the details of what it installs to are configured before it's built.
-- 
-- > data HardCodedUserInput = HardCodedUserInput (Maybe TargetDiskDevice) (Maybe DiskEraseConfirmed)
-- > 
-- > instance UserInput HardCodedUserInput where 
-- > 	targetDiskDevice (HardCodedUserInput t _) = Just t
-- > 	diskEraseConfirmed (HardCodedUserInput _ c) = Just c
-- > 
-- > userInput :: Version -> HardCodedUserInput
-- > userInput Installer =  HardCodedUserInput Nothing Nothing
-- > userInput Target = HardCodedUserInput (Just (TargetDiskDevice "/dev/sda")) (Just DiskEraseConfirmed)
--
-- For an example of how to use this to make an interactive installer,
-- see <https://git.joeyh.name/index.cgi/secret-project.git/>

module Propellor.Property.Installer.Target (
	-- * Main interface
	TargetPartTable(..),
	targetInstalled,
	fstabLists,
	-- * Additional properties
	mountTarget,
	targetBootable,
	partitionTargetDisk,
	-- * Utility functions
	targetDir,
	probeDisk,
	findDiskDevices,
	-- * Installation progress tracking
	TargetFilled,
	TargetFilledHandle,
	prepTargetFilled,
	checkTargetFilled,
	TargetFilledPercent(..),
	targetFilledPercent,
) where

import Propellor
import Propellor.Property.Installer.Types
import Propellor.Message
import Propellor.Types.Bootloader
import Propellor.Types.PartSpec
import Propellor.Property.Chroot
import Propellor.Property.Versioned
import Propellor.Property.Parted
import Propellor.Property.Mount
import qualified Propellor.Property.Fstab as Fstab
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.Rsync as Rsync

import Text.Read
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Directory
import System.FilePath
import Data.Maybe
import Data.List
import Data.Char
import Data.Ord
import Data.Ratio
import qualified Data.Semigroup as Sem
import System.Process (readProcess)

-- | Partition table for the target disk.
data TargetPartTable = TargetPartTable TableType [PartSpec DiskPart]

-- | Property that installs the target system to the TargetDiskDevice
-- specified in the UserInput. That device will be re-partitioned and
-- formatted and all files erased.
--
-- The installation is done efficiently by rsyncing the installer's files
-- to the target, which forms the basis for a chroot that is provisioned with
-- the specified version of the Host. Thanks to
-- Propellor.Property.Versioned, any unwanted properties of the installer
-- will be automatically reverted in the chroot.
--
-- When there is no TargetDiskDevice or the user has not confirmed the
-- installation, nothing is done except for installing dependencies. 
-- So, this can also be used as a property of the installer
-- image.
targetInstalled
	:: UserInput i 
	=> Versioned v Host
	-> v
	-> i
	-> TargetPartTable
	-> RevertableProperty (HasInfo + DebianLike) (HasInfo + DebianLike)
targetInstalled vtargethost v userinput (TargetPartTable tabletype partspec) = 
	case (targetDiskDevice userinput, diskEraseConfirmed userinput) of
		(Just (TargetDiskDevice targetdev), Just _diskeraseconfirmed) -> 
			go `describe` ("target system installed to " ++ targetdev)
		_ -> tightenTargets installdeps <!> doNothing
  where
	targethost = vtargethost `version` v
	go = RevertableProperty
		(setupRevertableProperty p)
		-- Versioned needs both "sides" of the RevertableProperty
		-- to have the same type, so add empty Info to make the
		-- types line up.
		(undoRevertableProperty p `setInfoProperty` mempty)
	  where
		p = partitionTargetDisk userinput tabletype partspec
			`before` mountTarget userinput partspec
			`before` provisioned chroot
	
	chroot = hostChroot targethost RsyncBootstrapper targetDir

	-- Install dependencies that will be needed later when installing
	-- the target.
	installdeps = Rsync.installed

data RsyncBootstrapper = RsyncBootstrapper

instance ChrootBootstrapper RsyncBootstrapper where
	buildchroot RsyncBootstrapper _ target = Right $
		mountaside
			`before` rsynced
			`before` umountaside
	  where
	  	-- bind mount the root filesystem to /mnt, which exposes
		-- the contents of all directories that have things mounted
		-- on top of them to rsync.
		mountaside = bindMount "/" "/mnt"
		rsynced = Rsync.rsync
			[ "--one-file-system"
			, "-aHAXS"
			, "--delete"
			, "/mnt/"
			, target
			]
		umountaside = cmdProperty "umount" ["-l", "/mnt"]
			`assume` MadeChange

-- | Gets the target mounted.
mountTarget
	:: UserInput i
	=> i
	-> [PartSpec DiskPart]
	-> RevertableProperty Linux Linux
mountTarget userinput partspec = setup <!> cleanup
  where
	setup = property "target mounted" $
		case targetDiskDevice userinput of
			Just (TargetDiskDevice targetdev) -> do
				liftIO unmountTarget
				r <- liftIO $ forM tomount $
					mountone targetdev
				if and r
					then return MadeChange
					else return FailedChange
			Nothing -> return NoChange
	cleanup = property "target unmounted" $ do
		liftIO unmountTarget
		liftIO $ removeDirectoryRecursive targetDir
		return NoChange

	-- Sort so / comes before /home etc
	tomount = sortOn (fst . fst) $
		map (\((mp, mo, _, _), n) -> ((mp, mo), n)) $
		zip partspec partNums

	mountone targetdev ((mmountpoint, mountopts), num) =
		case mmountpoint of
			Nothing -> return True
			Just mountpoint -> do
				let targetmount = targetDir ++ mountpoint
				createDirectoryIfMissing True targetmount
				let dev = diskPartition targetdev num
				mount "auto" dev targetmount mountopts

-- | Property for use in the target Host to set up its fstab.
-- Should be passed the same TargetPartTable as `targetInstalled`.
fstabLists
	:: UserInput i
	=> i
	-> TargetPartTable
	-> RevertableProperty Linux Linux
fstabLists userinput (TargetPartTable _ partspecs) = setup <!> doNothing
  where
	setup = case targetDiskDevice userinput of
		Just (TargetDiskDevice targetdev) ->
			Fstab.fstabbed mnts (swaps targetdev)
				`requires` devmounted
				`before` devumounted
		Nothing -> doNothing

	-- needed for ftabbed UUID probing to work
	devmounted :: Property Linux
	devmounted = tightenTargets $ mounted "devtmpfs" "udev" "/dev" mempty
	devumounted :: Property Linux
	devumounted = tightenTargets $ cmdProperty "umount" ["-l", "/dev"]
		`assume` MadeChange
	
	partitions = map (\(mp, _, mkpart, _) -> (mp, mkpart mempty)) partspecs
	mnts = mapMaybe fst $
		filter (\(_, p) -> partFs p /= Just LinuxSwap && partFs p /= Nothing) partitions
	swaps targetdev = 
		map (Fstab.SwapPartition . diskPartition targetdev . snd) $
			filter (\((_, p), _) -> partFs p == Just LinuxSwap)
				(zip partitions partNums)

-- | Make the target bootable using whatever bootloader is installed on it.
targetBootable
	:: UserInput i
	=> i
	-> RevertableProperty Linux Linux
targetBootable userinput = 
	case (targetDiskDevice userinput, diskEraseConfirmed userinput) of
		(Just (TargetDiskDevice targetdev), Just _diskeraseconfirmed) -> 
			go targetdev <!> doNothing
		_ -> doNothing <!> doNothing
  where
	desc = "bootloader installed on target disk"
	go :: FilePath -> Property Linux
	go targetdev = property' desc $ \w -> do
		bootloaders <- askInfo
		case bootloaders of
			[GrubInstalled gt] -> ensureProperty w $
				Grub.bootsMounted targetDir targetdev gt
			[] -> do
				warningMessage "no bootloader was installed"
				return NoChange
			l -> do
				warningMessage $ "don't know how to enable bootloader(s) " ++ show l
				return FailedChange

-- | Partitions the target disk.
partitionTargetDisk
	:: UserInput i
	=> i
	-> TableType
	-> [PartSpec DiskPart]
	-> RevertableProperty DebianLike DebianLike
partitionTargetDisk userinput tabletype partspec = go <!> doNothing
  where
	go = check targetNotMounted $ property' "target disk partitioned" $ \w -> do
		case (targetDiskDevice userinput, diskEraseConfirmed userinput) of
			(Just (TargetDiskDevice targetdev), Just _diskeraseconfirmed) -> do
				liftIO $ unmountTarget
				disksize <- liftIO $ getDiskSize targetdev
				let parttable = calcPartTable disksize tabletype safeAlignment partspec
				ensureProperty w $ 
					partitioned YesReallyDeleteDiskContents targetdev parttable
			_ -> error "user input does not allow partitioning disk"

unmountTarget :: IO ()
unmountTarget = mapM_ umountLazy . reverse . sort =<< targetMountPoints

targetMountPoints :: IO [MountPoint]
targetMountPoints = filter isTargetMountPoint <$> mountPoints

isTargetMountPoint :: MountPoint -> Bool
isTargetMountPoint mp = 
	mp == targetDir 
		|| addTrailingPathSeparator targetDir `isPrefixOf` mp

targetNotMounted :: IO Bool
targetNotMounted = not . any (== targetDir) <$> mountPoints

-- | Where the target disk is mounted while it's being installed.
targetDir :: FilePath
targetDir = "/target"

partNums :: [Integer]
partNums = [1..]

-- /dev/sda to /dev/sda1
diskPartition :: FilePath -> Integer -> FilePath
diskPartition dev num = dev ++ show num

-- | This can be used to find a likely disk device to use as the target
-- for an installation.
--
-- This is a bit of a hack; of course the user could be prompted but to
-- avoid prompting, some heuristics...
--   * It should not already be mounted. 
--   * Prefer disks big enough to comfortably hold a Linux installation,
--     so at least 8 gb.
--     (But, if the system only has a smaller disk, it should be used.)
--   * A medium size internal disk is better than a large removable disk,
--     because removable or added drives are often used for data storage
--     on systems with smaller internal disk for the OS.
--     (But, if the internal disk is too small, prefer removable disk;
--     some systems have an unusably small internal disk.)
--   * Prefer the first disk in BIOS order, all other things being equal,
--     because the main OS disk typically comes first. This can be
--     approximated by preferring /dev/sda to /dev/sdb.
probeDisk :: IO TargetDiskDevice
probeDisk = do
	unmountTarget
	mounteddevs <- getMountedDeviceIDs
	let notmounted d = flip notElem (map Just mounteddevs)
		<$> getMinorNumber d
	candidates <- mapM probeCandidate
		=<< filterM notmounted
		=<< findDiskDevices
	case reverse (sort candidates) of
		(Candidate { candidateDevice = Down dev } : _) -> 
			return $ TargetDiskDevice dev
		[] -> error "Unable to find any disk to install to!"

-- | Find disk devices, such as /dev/sda (not partitions)
findDiskDevices :: IO [FilePath]
findDiskDevices = map ("/dev" </>) . filter isdisk
	<$> getDirectoryContents "/dev"
  where
	isdisk ('s':'d':_:[]) = True
	isdisk _ = False

-- | When comparing two Candidates, the better of the two will be larger.
data Candidate = Candidate
	{ candidateBigEnoughForOS :: Bool
	, candidateIsFixedDisk :: Bool
	-- use Down so that /dev/sda orders larger than /dev/sdb
	, candidateDevice :: Down FilePath
	} deriving (Eq, Ord)

probeCandidate :: FilePath -> IO Candidate
probeCandidate dev = do
	DiskSize sz <- getDiskSize dev
	isfixeddisk <- not <$> isRemovableDisk dev
	return $ Candidate
		{ candidateBigEnoughForOS = sz >= 8 * onegb
		, candidateIsFixedDisk = isfixeddisk
		, candidateDevice = Down dev
		}
  where
	onegb = 1024*1024*1000

newtype MinorNumber = MinorNumber Integer
	deriving (Eq, Show)

getMountedDeviceIDs :: IO [MinorNumber]
getMountedDeviceIDs = mapMaybe parse . lines <$> readProcess "findmnt"
	[ "-rn"
	, "--output"
	, "MAJ:MIN"
	]
	""
  where
	parse = fmap MinorNumber . readMaybe 
		. dropWhile (not . isDigit) . dropWhile (/= ':')

-- There is not currently a native haskell interface for getting the minor
-- number of a device.
getMinorNumber :: FilePath -> IO (Maybe MinorNumber)
getMinorNumber dev = fmap MinorNumber . readMaybe 
	<$> readProcess "stat" [ "--printf", "%T", dev ] ""

-- A removable disk may show up as removable or as hotplug.
isRemovableDisk :: FilePath -> IO Bool
isRemovableDisk dev = do
	isremovable <- checkblk "RM"
	ishotplug <- checkblk "HOTPLUG"
	return (isremovable || ishotplug)
  where
	checkblk field = (== "1\n") <$> readProcess "lsblk"
		[ "-rn"
		, "--nodeps"
		, "--output", field
		, dev
		]
		""

getDiskSize :: FilePath -> IO DiskSize
getDiskSize dev = do
	sectors <- fromMaybe 0 . readMaybe 
		<$> readProcess "blockdev" ["--getsz", dev] ""
	return (DiskSize (sectors * 512))

getMountsSizes :: IO [(MountPoint, Integer)]
getMountsSizes = mapMaybe (parse . words) . lines <$> readProcess "findmnt" ps ""
  where
	ps = ["-rnb", "-o", "TARGET,USED"]
	parse (mp:szs:[]) = do
		sz <- readMaybe szs
		return (mp, sz)
	parse _ = Nothing

-- | How much of the target disks are used, compared with the size of the
-- installer's root device. Since the main part of an installation
-- is `targetInstalled` rsyncing the latter to the former, this allows
-- roughly estimating the percent done while an install is running,
-- and can be used in some sort of progress display.
data TargetFilled = TargetFilled (Ratio Integer)
	deriving (Show, Eq)

instance Sem.Semigroup TargetFilled where
	TargetFilled n <> TargetFilled m = TargetFilled (n+m) 

instance Monoid TargetFilled where
	mempty = TargetFilled (0 % 1)
	mappend = (Sem.<>)

newtype TargetFilledHandle = TargetFilledHandle Integer

-- | Prepare for getting `TargetFilled`.
prepTargetFilled :: IO TargetFilledHandle
prepTargetFilled = go =<< getMountSource "/"
  where
	go (Just dev) = do
		-- Assumes that the installer uses a single partition.
		DiskSize sz <- getDiskSize dev
		return (TargetFilledHandle sz)
	go Nothing = return (TargetFilledHandle 0)

-- | Get the current `TargetFilled` value. This is fast enough to be run
-- multiple times per second without using much CPU.
checkTargetFilled :: TargetFilledHandle -> IO TargetFilled
checkTargetFilled (TargetFilledHandle installsz) = do
	targetsz <- sum . map snd . filter (isTargetMountPoint . fst)
		<$> getMountsSizes
	return (TargetFilled (targetsz % max 1 installsz))

newtype TargetFilledPercent = TargetFilledPercent Int
	deriving (Show, Eq)

targetFilledPercent :: TargetFilled -> TargetFilledPercent
targetFilledPercent (TargetFilled r) = TargetFilledPercent $ floor percent
  where
	percent :: Double
	percent = min 100 (fromRational r * 100)
