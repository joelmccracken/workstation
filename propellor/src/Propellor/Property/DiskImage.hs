-- | Disk image generation.
--
-- This module is designed to be imported unqualified.

{-# LANGUAGE TypeFamilies #-}

module Propellor.Property.DiskImage (
	-- * Partition specification
	module Propellor.Property.DiskImage.PartSpec,
	-- * Properties
	DiskImage(..),
	RawDiskImage(..),
	VirtualBoxPointer(..),
	imageBuilt,
	imageRebuilt,
	imageBuiltFor,
	imageRebuiltFor,
	imageBuiltFrom,
	imageExists,
	imageChrootNotPresent,
	GrubTarget(..),
	noBootloader,
) where

import Propellor.Base
import Propellor.Property.DiskImage.PartSpec
import Propellor.Property.Chroot (Chroot)
import Propellor.Property.Chroot.Util (removeChroot)
import Propellor.Property.Mount
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.FlashKernel as FlashKernel
import Propellor.Property.Parted
import Propellor.Property.Fstab (SwapPartition(..), genFstab)
import Propellor.Property.Partition
import Propellor.Property.Rsync
import Propellor.Types.Info
import Propellor.Types.Bootloader
import Propellor.Container
import Utility.Path
import Utility.DataUnits

import Data.List (isPrefixOf, isInfixOf, sortBy, unzip4)
import Data.Function (on)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import System.Posix.Files

-- | Type class of disk image formats.
class DiskImage d where
	-- | Get the location where the raw disk image should be stored.
	rawDiskImage :: d -> RawDiskImage
	-- | Describe the disk image (for display to the user)
	describeDiskImage :: d -> String
	-- | Convert the raw disk image file in the
	-- `rawDiskImage` location into the desired disk image format.
	-- For best efficiency, the raw disk imasge file should be left
	-- unchanged on disk.
	buildDiskImage :: d -> RevertableProperty DebianLike Linux

-- | A raw disk image, that can be written directly out to a disk.
newtype RawDiskImage = RawDiskImage FilePath

instance DiskImage RawDiskImage where
	rawDiskImage = id
	describeDiskImage (RawDiskImage f) = f
	buildDiskImage (RawDiskImage _) = doNothing <!> doNothing

-- | A virtualbox .vmdk file, which contains a pointer to the raw disk
-- image. This can be built very quickly.
newtype VirtualBoxPointer = VirtualBoxPointer FilePath

instance DiskImage VirtualBoxPointer where
	rawDiskImage (VirtualBoxPointer f) = RawDiskImage $
		dropExtension f ++ ".img"
	describeDiskImage (VirtualBoxPointer f) = f
	buildDiskImage (VirtualBoxPointer vmdkfile) = (setup <!> cleanup)
		`describe` (vmdkfile ++ " built")
	  where
		setup = cmdProperty "VBoxManage"
			[ "internalcommands", "createrawvmdk"
			, "-filename", vmdkfile
			, "-rawdisk", diskimage
			]
			`changesFile` vmdkfile
			`onChange` File.mode vmdkfile (combineModes (ownerWriteMode : readModes))
			`requires` Apt.installed ["virtualbox"]
			`requires` File.notPresent vmdkfile
		cleanup = tightenTargets $ File.notPresent vmdkfile
		RawDiskImage diskimage = rawDiskImage (VirtualBoxPointer vmdkfile)

-- | Creates a bootable disk image.
--
-- First the specified Chroot is set up, and its properties are satisfied.
--
-- Then, the disk image is set up, and the chroot is copied into the
-- appropriate partition(s) of it. 
--
-- The partitions default to being sized just large enough to fit the files
-- from the chroot. You can use `addFreeSpace` to make them a bit larger
-- than that, or `setSize` to use a fixed size.
-- 
-- Note that the disk image file is reused if it already exists,
-- to avoid expensive IO to generate a new one. And, it's updated in-place,
-- so its contents are undefined during the build process.
--
-- Note that the `Service.noServices` property is automatically added to the
-- chroot while the disk image is being built, which should prevent any
-- daemons that are included from being started on the system that is
-- building the disk image.
--
-- Example use:
--
-- > import Propellor.Property.DiskImage
-- > import Propellor.Property.Chroot
-- > 
-- > foo = host "foo.example.com" $ props
-- > 	& imageBuilt (RawDiskImage "/srv/diskimages/disk.img") mychroot
-- >		MSDOS
-- >		[ partition EXT2 `mountedAt` "/boot"
-- >			`setFlag` BootFlag
-- >		, partition EXT4 `mountedAt` "/"
-- >			`addFreeSpace` MegaBytes 100
-- >			`mountOpt` errorReadonly
-- >		, swapPartition (MegaBytes 256)
-- >		]
-- >  where
-- >	mychroot d = debootstrapped mempty d $ props
-- >		& osDebian Unstable X86_64
-- >		& Apt.installed ["linux-image-amd64"]
-- >		& Grub.installed PC
-- >		& User.hasPassword (User "root")
-- >		& User.accountFor (User "demo")
-- > 		& User.hasPassword (User "demo")
-- >		& User.hasDesktopGroups (User "demo")
-- > 		& ...
imageBuilt :: DiskImage d => d -> (FilePath -> Chroot) -> TableType -> [PartSpec ()] -> RevertableProperty (HasInfo + DebianLike) Linux
imageBuilt = imageBuilt' False

-- | Like 'imageBuilt', but the chroot is deleted and rebuilt from scratch
-- each time. This is more expensive, but useful to ensure reproducible
-- results when the properties of the chroot have been changed.
imageRebuilt :: DiskImage d => d -> (FilePath -> Chroot) -> TableType -> [PartSpec ()] -> RevertableProperty (HasInfo + DebianLike) Linux
imageRebuilt = imageBuilt' True

-- | Create a bootable disk image for a Host.
--
-- This works just like 'imageBuilt', but partition table is
-- determined by looking at the Host's 'hasPartitionTableType',
-- `hasPartition', and 'adjustPartition' properties.
--
-- For example:
--
-- > foo :: Host
-- > foo = host "foo.example.com" $ props
-- >	& imageBuiltFor bar
-- >		(RawDiskImage "/srv/diskimages/bar-disk.img")
-- >		(Debootstrapped mempty)
-- >
-- > bar :: Host
-- > bar = host "bar.example.com" $ props
-- >	& hasPartiton
-- >		( partition EXT2
-- >		`mountedAt` "/boot"
-- >		`partLocation` Beginning
-- >		`addFreeSpace` MegaBytes 150
-- >		)
-- >	& hasPartiton
-- >		( partition EXT4
-- >		`mountedAt` "/"
-- >		`addFreeSpace` MegaBytes 500
-- >		)
-- >	& osDebian Unstable X86_64
-- >	& Apt.installed ["linux-image-amd64"]
-- >	& Grub.installed PC
-- >	& hasPassword (User "root")
imageBuiltFor :: (DiskImage d, Chroot.ChrootBootstrapper bootstrapper) => Host -> d -> bootstrapper -> RevertableProperty (HasInfo + DebianLike) Linux
imageBuiltFor = imageBuiltFor' False

-- | Like 'imageBuiltFor', but the chroot is deleted and rebuilt from
-- scratch each time.
imageRebuiltFor :: (DiskImage d, Chroot.ChrootBootstrapper bootstrapper) => Host -> d -> bootstrapper -> RevertableProperty (HasInfo + DebianLike) Linux
imageRebuiltFor = imageBuiltFor' False

imageBuiltFor' :: (DiskImage d, Chroot.ChrootBootstrapper bootstrapper) => Bool -> Host -> d -> bootstrapper -> RevertableProperty (HasInfo + DebianLike) Linux
imageBuiltFor' rebuild h d bs =
	imageBuilt' rebuild d (Chroot.hostChroot h bs) tt pil
  where
	PartTableSpec tt pil = toPartTableSpec (fromInfo (hostInfo h))

imageBuilt' :: DiskImage d => Bool -> d -> (FilePath -> Chroot) -> TableType -> [PartSpec ()] -> RevertableProperty (HasInfo + DebianLike) Linux
imageBuilt' rebuild img mkchroot tabletype partspec =
	imageBuiltFrom img chrootdir tabletype final partspec
		`requires` Chroot.provisioned chroot
		`requires` (cleanrebuild <!> (doNothing :: Property UnixLike))
		`describe` desc
  where
	desc = "built disk image " ++ describeDiskImage img
	cleanrebuild :: Property Linux
	cleanrebuild
		| rebuild = property desc $ do
			liftIO $ removeChroot chrootdir
			return MadeChange
		| otherwise = doNothing
	chrootdir = imageChroot img
	chroot =
		let c = propprivdataonly $ mkchroot chrootdir
		in setContainerProps c $ containerProps c
			-- Before ensuring any other properties of the chroot,
			-- avoid starting services. Reverted by imageFinalized.
			&^ Service.noServices
			& cachesCleaned
	-- Only propagate privdata Info from this chroot, nothing else.
	propprivdataonly (Chroot.Chroot d b ip h) =
		Chroot.Chroot d b (\c _ -> ip c onlyPrivData) h
	-- Pick boot loader finalization based on which bootloader is
	-- installed.
	final = case fromInfo (containerInfo chroot) of
		[] -> unbootable "no bootloader is installed"
		[GrubInstalled grubtarget] -> grubFinalized grubtarget
		[UbootInstalled p] -> ubootFinalized p
		[FlashKernelInstalled] -> flashKernelFinalized
		[UbootInstalled p, FlashKernelInstalled] -> 
			ubootFlashKernelFinalized p
		[FlashKernelInstalled, UbootInstalled p] -> 
			ubootFlashKernelFinalized p
		[NoBootloader] -> noBootloaderFinalized
		_ -> unbootable "multiple bootloaders are installed; don't know which to use"

-- | This property is automatically added to the chroot when building a
-- disk image. It cleans any caches of information that can be omitted;
-- eg the apt cache on Debian.
cachesCleaned :: Property UnixLike
cachesCleaned = "cache cleaned" ==> (Apt.cacheCleaned `pickOS` skipit)
  where
	skipit = doNothing :: Property UnixLike

-- | Builds a disk image from the contents of a chroot.
imageBuiltFrom :: DiskImage d => d -> FilePath -> TableType -> Finalization -> [PartSpec ()] -> RevertableProperty (HasInfo + DebianLike) Linux
imageBuiltFrom img chrootdir tabletype final partspec = mkimg <!> rmimg
  where
	desc = describeDiskImage img ++ " built from " ++ chrootdir
	dest@(RawDiskImage imgfile) = rawDiskImage img
	mkimg = property' desc $ \w -> do
		-- Unmount helper filesystems such as proc from the chroot
		-- first; don't want to include the contents of those.
		liftIO $ unmountBelow chrootdir
		szm <- M.mapKeys (toSysDir chrootdir) . M.map toPartSize
			<$> liftIO (dirSizes chrootdir)
		let calcsz mnts = maybe defSz fudgeSz . getMountSz szm mnts
		-- tie the knot!
		let (mnts, mntopts, parttable) = fitChrootSize tabletype partspec $
			map (calcsz mnts) mnts
		ensureProperty w $
			imageExists' dest parttable
				`before`
			kpartx imgfile (mkimg' mnts mntopts parttable)
				`before`
			buildDiskImage img
	mkimg' mnts mntopts parttable devs =
		partitionsPopulated chrootdir mnts mntopts devs
			`before`
		imageFinalized final dest mnts mntopts devs parttable
	rmimg = undoRevertableProperty (buildDiskImage img)
		`before` undoRevertableProperty (imageExists' dest dummyparttable)
	dummyparttable = PartTable tabletype safeAlignment []

partitionsPopulated :: FilePath -> [Maybe MountPoint] -> [MountOpts] -> [LoopDev] -> Property DebianLike
partitionsPopulated chrootdir mnts mntopts devs = property' desc $ \w ->
	mconcat $ zipWith3 (go w) mnts mntopts devs
  where
	desc = "partitions populated from " ++ chrootdir

	go _ Nothing _ _ = noChange
	go w (Just mnt) mntopt loopdev = ifM (liftIO $ doesDirectoryExist srcdir) $
		( withTmpDir "mnt" $ \tmpdir -> bracket
			(liftIO $ mount "auto" (partitionLoopDev loopdev) tmpdir mntopt)
			(const $ liftIO $ umountLazy tmpdir)
			$ \ismounted -> if ismounted
				then ensureProperty w $
					syncDirFiltered (filtersfor mnt) srcdir tmpdir
				else return FailedChange
		, return NoChange
		)
	  where
		srcdir = chrootdir ++ mnt

	filtersfor mnt =
		let childmnts = map (drop (length (dropTrailingPathSeparator mnt))) $
			filter (\m -> m /= mnt && addTrailingPathSeparator mnt `isPrefixOf` m)
				(catMaybes mnts)
		in concatMap (\m ->
			-- Include the child mount point, but exclude its contents.
			[ Include (Pattern m)
			, Exclude (filesUnder m)
			-- Preserve any lost+found directory that mkfs made
			, Protect (Pattern "lost+found")
			]) childmnts

-- The constructor for each Partition is passed the size of the files
-- from the chroot that will be put in that partition.
fitChrootSize :: TableType -> [PartSpec ()] -> [PartSize] -> ([Maybe MountPoint], [MountOpts], PartTable)
fitChrootSize tt l basesizes = (mounts, mountopts, parttable)
  where
	(mounts, mountopts, sizers, _) = unzip4 l
	parttable = PartTable tt safeAlignment (zipWith id sizers basesizes)

-- | Generates a map of the sizes of the contents of
-- every directory in a filesystem tree.
--
-- (Hard links are counted multiple times for simplicity)
--
-- Should be same values as du -bl
dirSizes :: FilePath -> IO (M.Map FilePath Integer)
dirSizes top = go M.empty top [top]
  where
	go m _ [] = return m
	go m dir (i:is) = flip catchIO (\_ioerr -> go m dir is) $ do
		s <- getSymbolicLinkStatus i
		let sz = fromIntegral (fileSize s)
		if isDirectory s
			then do
				subm <- go M.empty i =<< dirContents i
				let sz' = M.foldr' (+) sz
					(M.filterWithKey (const . subdirof i) subm)
				go (M.insertWith (+) i sz' (M.union m subm)) dir is
			else go (M.insertWith (+) dir sz m) dir is
	subdirof parent i = not (i `equalFilePath` parent) && takeDirectory i `equalFilePath` parent

getMountSz :: (M.Map FilePath PartSize) -> [Maybe MountPoint] -> Maybe MountPoint -> Maybe PartSize
getMountSz _ _ Nothing = Nothing
getMountSz szm l (Just mntpt) =
	fmap (`reducePartSize` childsz) (M.lookup mntpt szm)
  where
	childsz = mconcat $ mapMaybe (getMountSz szm l) (filter (isChild mntpt) l)

-- | Ensures that a disk image file of the specified size exists.
--
-- If the file doesn't exist, or is too small, creates a new one, full of 0's.
--
-- If the file is too large, truncates it down to the specified size.
imageExists :: RawDiskImage -> ByteSize -> Property Linux
imageExists (RawDiskImage img) isz = property ("disk image exists" ++ img) $ liftIO $ do
	ms <- catchMaybeIO $ getFileStatus img
	case fmap (toInteger . fileSize) ms of
		Just s
			| s == toInteger sz -> return NoChange
			| s > toInteger sz -> do
				infoMessage ["truncating " ++ img ++ " to " ++ humansz]
				setFileSize img (fromInteger sz)
				return MadeChange
			| otherwise -> do
				infoMessage ["expanding " ++ img ++ " from " ++ roughSize storageUnits False s ++ " to " ++ humansz]
				L.writeFile img (L.replicate (fromIntegral sz) 0)
				return MadeChange
		Nothing -> do
			infoMessage ["creating " ++ img ++ " of size " ++ humansz]
			L.writeFile img (L.replicate (fromIntegral sz) 0)
			return MadeChange
  where
	sz = ceiling (fromInteger isz / sectorsize) * ceiling sectorsize
	humansz = roughSize storageUnits False (toInteger sz)
	-- Disks have a sector size, and making a disk image not
	-- aligned to a sector size will confuse some programs.
	-- Common sector sizes are 512 and 4096; use 4096 as it's larger.
	sectorsize = 4096 :: Double

-- | Ensure that disk image file exists and is partitioned.
--
-- Avoids repartitioning the disk image, when a file of the right size
-- already exists, and it has the same PartTable.
imageExists' :: RawDiskImage -> PartTable -> RevertableProperty DebianLike UnixLike
imageExists' dest@(RawDiskImage img) parttable = (setup <!> cleanup) `describe` desc
  where
	desc = "disk image exists " ++ img
	parttablefile = imageParttableFile dest
	setup = property' desc $ \w -> do
		oldparttable <- liftIO $ catchDefaultIO "" $ readFileStrict parttablefile
		res <- ensureProperty w $ imageExists dest (partTableSize parttable)
		if res == NoChange && oldparttable == show parttable
			then return NoChange
			else if res == FailedChange
				then return FailedChange
				else do
					liftIO $ writeFile parttablefile (show parttable)
					ensureProperty w $ partitioned YesReallyDeleteDiskContents img parttable
	cleanup = File.notPresent img
		`before`
		File.notPresent parttablefile

-- | A property that is run after the disk image is created, with
-- its populated partition tree mounted in the provided
-- location from the provided loop devices. This is typically used to
-- install a boot loader in the image's superblock.
--
-- It's ok if the property leaves additional things mounted
-- in the partition tree.
type Finalization = (RawDiskImage -> FilePath -> [LoopDev] -> Property Linux)

imageFinalized :: Finalization -> RawDiskImage -> [Maybe MountPoint] -> [MountOpts] -> [LoopDev] -> PartTable -> Property Linux
imageFinalized final img mnts mntopts devs (PartTable _ _ parts) =
	property' "disk image finalized" $ \w ->
		withTmpDir "mnt" $ \top ->
			go w top `finally` liftIO (unmountall top)
  where
	go w top = do
		liftIO $ mountall top
		liftIO $ writefstab top
		liftIO $ allowservices top
		ensureProperty w $ 
			final img top devs

	-- Ordered lexographically by mount point, so / comes before /usr
	-- comes before /usr/local
	orderedmntsdevs :: [(Maybe MountPoint, (MountOpts, LoopDev))]
	orderedmntsdevs = sortBy (compare `on` fst) $ zip mnts (zip mntopts devs)

	swaps = map (SwapPartition . partitionLoopDev . snd) $
		filter ((== Just LinuxSwap) . partFs . fst) $
			zip parts devs

	mountall top = forM_ orderedmntsdevs $ \(mp, (mopts, loopdev)) -> case mp of
		Nothing -> noop
		Just p -> do
			let mnt = top ++ p
			createDirectoryIfMissing True mnt
			unlessM (mount "auto" (partitionLoopDev loopdev) mnt mopts) $
				error $ "failed mounting " ++ mnt

	unmountall top = do
		unmountBelow top
		umountLazy top

	writefstab top = do
		let fstab = top ++ "/etc/fstab"
		old <- catchDefaultIO [] $ filter (not . unconfigured) . lines
			<$> readFileStrict fstab
		new <- genFstab (map (top ++) (catMaybes mnts))
			swaps (toSysDir top)
		writeFile fstab $ unlines $ new ++ old
	-- Eg "UNCONFIGURED FSTAB FOR BASE SYSTEM"
	unconfigured s = "UNCONFIGURED" `isInfixOf` s

	allowservices top = nukeFile (top ++ "/usr/sbin/policy-rc.d")

unbootable :: String -> Finalization
unbootable msg = \_ _ _ -> property desc $ do
	warningMessage (desc ++ ": " ++ msg)
	return FailedChange
  where
	desc = "image is not bootable"

grubFinalized :: GrubTarget -> Finalization
grubFinalized grubtarget _img mnt loopdevs = 
	Grub.bootsMounted mnt wholediskloopdev grubtarget
		`describe` "disk image boots using grub"
  where
	-- It doesn't matter which loopdev we use; all
	-- come from the same disk image, and it's the loop dev
	-- for the whole disk image we seek.
	wholediskloopdev = case loopdevs of
		(l:_) -> wholeDiskLoopDev l
		[] -> error "No loop devs provided!"

ubootFinalized :: (FilePath -> FilePath -> Property Linux) -> Finalization
ubootFinalized p (RawDiskImage img) mnt _loopdevs = p img mnt

flashKernelFinalized :: Finalization
flashKernelFinalized _img mnt _loopdevs = FlashKernel.flashKernelMounted mnt

ubootFlashKernelFinalized :: (FilePath -> FilePath -> Property Linux) -> Finalization
ubootFlashKernelFinalized p img mnt loopdevs = 
	ubootFinalized p img mnt loopdevs
		`before` flashKernelFinalized img mnt loopdevs

-- | Normally a boot loader is installed on a disk image. However,
-- when the disk image will be booted by eg qemu booting the kernel and
-- initrd, no boot loader is needed, and this property can be used.
noBootloader :: Property (HasInfo + UnixLike)
noBootloader = pureInfoProperty "no bootloader" [NoBootloader]

noBootloaderFinalized :: Finalization
noBootloaderFinalized _img _mnt _loopDevs = doNothing

imageChrootNotPresent :: DiskImage d => d -> Property UnixLike
imageChrootNotPresent img = check (doesDirectoryExist dir) $
	property "destroy the chroot used to build the image" $ makeChange $ do
		removeChroot dir
		nukeFile $ imageParttableFile img
  where
	dir = imageChroot img

imageChroot :: DiskImage d => d -> FilePath
imageChroot img = imgfile <.> "chroot"
  where
	RawDiskImage imgfile = rawDiskImage img

imageParttableFile :: DiskImage d => d -> FilePath
imageParttableFile img = imgfile <.> "parttable"
  where
	RawDiskImage imgfile = rawDiskImage img

isChild :: FilePath -> Maybe MountPoint -> Bool
isChild mntpt (Just d)
	| d `equalFilePath` mntpt = False
	| otherwise = mntpt `dirContains` d
isChild _ Nothing = False

-- | From a location in a chroot (eg, /tmp/chroot/usr) to
-- the corresponding location inside (eg, /usr).
toSysDir :: FilePath -> FilePath -> FilePath
toSysDir chrootdir d = case makeRelative chrootdir d of
		"." -> "/"
		sysdir -> "/" ++ sysdir
