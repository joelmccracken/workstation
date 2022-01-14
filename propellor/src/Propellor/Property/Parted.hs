{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Parted (
	-- * Types
	TableType(..),
	PartTable(..),
	partTableSize,
	Partition(..),
	mkPartition,
	Partition.Fs(..),
	PartSize(..),
	ByteSize,
	toPartSize,
	fromPartSize,
	reducePartSize,
	Alignment(..),
	safeAlignment,
	Partition.MkfsOpts,
	PartType(..),
	PartFlag(..),
	-- * Properties
	partitioned,
	parted,
	Eep(..),
	installed,
	-- * Partition table sizing
	calcPartTable,
	DiskSize(..),
	DiskPart,
	DiskSpaceUse(..),
	useDiskSpace,
	defSz,
	fudgeSz,
) where

import Propellor.Base
import Propellor.Property.Parted.Types
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Pacman as Pacman
import qualified Propellor.Property.Partition as Partition
import Propellor.Types.PartSpec (PartSpec)
import Utility.DataUnits

import System.Posix.Files
import qualified Data.Semigroup as Sem
import Data.List (genericLength)

data Eep = YesReallyDeleteDiskContents

-- | Partitions a disk using parted, and formats the partitions.
--
-- The FilePath can be a block device (eg, \/dev\/sda), or a disk image file.
--
-- This deletes any existing partitions in the disk! Use with EXTREME caution!
partitioned :: Eep -> FilePath -> PartTable -> Property DebianLike
partitioned eep disk parttable@(PartTable _ _ parts) = property' desc $ \w -> do
	isdev <- liftIO $ isBlockDevice <$> getFileStatus disk
	ensureProperty w $ combineProperties desc $ props
		& parted eep disk (fst (calcPartedParamsSize parttable))
		& if isdev
			then formatl (map (\n -> disk ++ show n) [1 :: Int ..])
			else Partition.kpartx disk (formatl . map Partition.partitionLoopDev)
  where
	desc = disk ++ " partitioned"
	formatl devs = combineProperties desc (toProps $ map format (zip parts devs))
	format (p, dev) = case partFs p of
		Just fs -> Partition.formatted' (partMkFsOpts p)
			Partition.YesReallyFormatPartition fs dev
		Nothing -> doNothing

-- | Gets the total size of the disk specified by the partition table.
partTableSize :: PartTable -> ByteSize
partTableSize = snd . calcPartedParamsSize

calcPartedParamsSize :: PartTable -> ([String], ByteSize)
calcPartedParamsSize (PartTable tabletype alignment parts) = 
	let (ps, sz) = calcparts (1 :: Integer) firstpos parts []
	in (concat (mklabel : ps), sz)
  where
	mklabel = ["mklabel", pval tabletype]
	mkflag partnum (f, b) =
		[ "set"
		, show partnum
		, pval f
		, pval b
		]
	mkpart partnum startpos endpos p = catMaybes
		[ Just "mkpart"
		, Just $ pval (partType p)
		, fmap pval (partFs p)
		, Just $ partposexact startpos
		, Just $ partposfuzzy endpos
		] ++ case partName p of
			Just n -> ["name", show partnum, n]
			Nothing -> []
	calcparts partnum startpos (p:ps) c =
		let endpos = startpos + align (partSize p)
		in calcparts (partnum+1) endpos ps
			(c ++ mkpart partnum startpos (endpos-1) p : map (mkflag partnum) (partFlags p))
	calcparts _ endpos [] c = (c, endpos)

	-- Exact partition position value for parted.
	-- For alignment to work, the start of a partition must be
	-- specified exactly.
	partposexact n
		| n > 0 = show n ++ "B"
		-- parted can't make partitions smaller than 1MB;
		-- avoid failure in edge cases
		| otherwise = "1MB"
	
	-- Fuzzy partition position valie for parted.
	-- This is used to specify the end of the partition,
	-- parted takes the "MB" as license to slightly reduce the
	-- partition size when something about the partition table
	-- does not allow the partition to end exactly at the position.
	partposfuzzy n
		| n > 0 = show (fromIntegral n / 1000000 :: Double) ++ "MB"
		| otherwise = "1MB"

	-- Location of the start of the first partition,
	-- leaving space for the partition table, and aligning.
	firstpos = align partitionTableOverhead
	
	align = alignTo alignment

-- | Runs parted on a disk with the specified parameters.
--
-- Parted is run in script mode, so it will never prompt for input.
parted :: Eep -> FilePath -> [String] -> Property (DebianLike + ArchLinux)
parted YesReallyDeleteDiskContents disk ps = p `requires` installed
  where
	p = cmdProperty "parted" ("--script":"--align":"none":disk:ps)
		`assume` MadeChange

-- | Gets parted installed.
installed :: Property (DebianLike + ArchLinux)
installed = Apt.installed ["parted"] `pickOS` Pacman.installed ["parted"]

-- | Some disk is used to store the partition table itself. Assume less
-- than 1 mb.
partitionTableOverhead :: PartSize
partitionTableOverhead = MegaBytes 1

-- | Calculate a partition table, for a given size of disk.
--
-- For example:
--
-- >	calcPartTable (DiskSize (1024 * 1024 * 1024 * 100)) MSDOS safeAlignment
-- > 		[ partition EXT2 `mountedAt` "/boot"
-- > 			`setSize` MegaBytes 256
-- > 			`setFlag` BootFlag
-- >		, partition EXT4 `mountedAt` "/"
-- >			`useDiskSpace` RemainingSpace
-- >		]
calcPartTable :: DiskSize -> TableType -> Alignment -> [PartSpec DiskPart] -> PartTable
calcPartTable (DiskSize disksize) tt alignment l =
	PartTable tt alignment (map go l)
  where
	go (_, _, mkpart, FixedDiskPart) = mkpart defSz
	go (_, _, mkpart, DynamicDiskPart (Percent p)) = mkpart $ Bytes $
		diskremainingafterfixed * fromIntegral p `div` 100
	go (_, _, mkpart, DynamicDiskPart RemainingSpace) = mkpart $ Bytes $
		diskremaining `div` genericLength (filter isremainingspace l)
	diskremainingafterfixed =
		disksize - sumsizes (filter isfixed l)
	diskremaining =
		disksize - sumsizes (filter (not . isremainingspace) l)
	sumsizes = partTableSize . PartTable tt alignment . map go
	isfixed (_, _, _, FixedDiskPart) = True
	isfixed _ = False
	isremainingspace (_, _, _, DynamicDiskPart RemainingSpace) = True
	isremainingspace _ = False

-- | Size of a disk, in bytes.
newtype DiskSize = DiskSize ByteSize
	deriving (Show)

data DiskPart = FixedDiskPart | DynamicDiskPart DiskSpaceUse

data DiskSpaceUse = Percent Int | RemainingSpace

instance Sem.Semigroup DiskPart where
	FixedDiskPart <> FixedDiskPart = FixedDiskPart
	DynamicDiskPart (Percent a) <> DynamicDiskPart (Percent b) =
		DynamicDiskPart (Percent (a + b))
	DynamicDiskPart RemainingSpace <> DynamicDiskPart RemainingSpace = 
		DynamicDiskPart RemainingSpace
	DynamicDiskPart (Percent a) <> _ = DynamicDiskPart (Percent a)
	_ <> DynamicDiskPart (Percent b) = DynamicDiskPart (Percent b)
	DynamicDiskPart RemainingSpace <> _ = DynamicDiskPart RemainingSpace
	_ <> DynamicDiskPart RemainingSpace = DynamicDiskPart RemainingSpace

instance Monoid DiskPart
  where
	mempty = FixedDiskPart
	mappend = (Sem.<>)

-- | Make a partition use some percentage of the size of the disk
-- (less all fixed size partitions), or the remaining space in the disk.
useDiskSpace :: PartSpec DiskPart -> DiskSpaceUse -> PartSpec DiskPart
useDiskSpace (mp, o, p, _) diskuse = (mp, o, p, DynamicDiskPart diskuse)

-- | Default partition size when not otherwize specified is 128 MegaBytes.
defSz :: PartSize
defSz = MegaBytes 128

-- | When a partition is sized to fit the files that live in it,
-- this fudge factor is added to the size of the files. This is necessary
-- since filesystems have some space overhead.
-- 
-- Add 2% for filesystem overhead. Rationalle for picking 2%:
-- A filesystem with 1% overhead might just sneak by as acceptable.
-- Double that just in case. Add an additional 3 mb to deal with
-- non-scaling overhead of filesystems (eg, superblocks). 
-- Add an additional 200 mb for temp files, journals, etc.
fudgeSz :: PartSize -> PartSize
fudgeSz (MegaBytes n) = MegaBytes (n + n `div` 100 * 2 + 3 + 200)
fudgeSz (Bytes n) = fudgeSz (toPartSize n)

alignTo :: Alignment -> PartSize -> ByteSize
alignTo _ (Bytes n) = n -- no alignment done for Bytes
alignTo (Alignment alignment) partsize
	| alignment < 1 = n
	| otherwise = case rem n alignment of
		0 -> n
		r -> n - r + alignment
  where
	n = fromPartSize partsize
