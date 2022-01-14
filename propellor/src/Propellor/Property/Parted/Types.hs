module Propellor.Property.Parted.Types where

import qualified Propellor.Property.Partition as Partition
import Utility.DataUnits

import Data.Char
import qualified Data.Semigroup as Sem
import Data.Monoid
import Prelude

class PartedVal a where
	pval :: a -> String

-- | Types of partition tables supported by parted.
data TableType = MSDOS | GPT | AIX | AMIGA | BSD | DVH | LOOP | MAC | PC98 | SUN
	deriving (Show)

instance PartedVal TableType where
	pval = map toLower . show

-- | A disk's partition table.
data PartTable = PartTable TableType Alignment [Partition]
	deriving (Show)

instance Sem.Semigroup PartTable where
	-- | uses the TableType of the second parameter
	-- and the larger alignment,
	PartTable _l1 a1 ps1 <> PartTable l2 a2 ps2 =
		PartTable l2 (max a1 a2) (ps1 ++ ps2)

instance Monoid PartTable where
	-- | default TableType is MSDOS, with a `safeAlignment`.
	mempty = PartTable MSDOS safeAlignment []
	mappend = (Sem.<>)

-- | A partition on the disk.
data Partition = Partition
	{ partType :: PartType
	, partSize :: PartSize
	, partFs :: Maybe Partition.Fs
	, partMkFsOpts :: Partition.MkfsOpts
	, partFlags :: [(PartFlag, Bool)] -- ^ flags can be set or unset (parted may set some flags by default)
	, partName :: Maybe String -- ^ optional name for partition (only works for GPT, PC98, MAC)
	}
	deriving (Show)

-- | Makes a Partition with defaults for non-important values.
mkPartition :: Maybe Partition.Fs -> PartSize -> Partition
mkPartition fs sz = Partition
	{ partType = Primary
	, partSize = sz
	, partFs = fs
	, partMkFsOpts = []
	, partFlags = []
	, partName = Nothing
	}

-- | Type of a partition.
data PartType = Primary | Logical | Extended
	deriving (Show)

instance PartedVal PartType where
	pval Primary = "primary"
	pval Logical = "logical"
	pval Extended = "extended"

-- | Size of a partition.
data PartSize
	-- Since disk sizes are typically given in MB, not MiB, this
	-- uses SI MegaBytes (powers of 10).
	= MegaBytes Integer
	-- For more control, the partition size can be given in bytes.
	-- Note that this will prevent any automatic alignment from 
	-- being done.
	| Bytes Integer
	deriving (Show)

-- | Rounds up to the nearest MegaByte.
toPartSize :: ByteSize -> PartSize
toPartSize = toPartSize' ceiling

toPartSize' :: (Double -> Integer) -> ByteSize -> PartSize
toPartSize' rounder b = MegaBytes $ rounder (fromInteger b / 1000000 :: Double)

fromPartSize :: PartSize -> ByteSize
fromPartSize (MegaBytes b) = b * 1000000
fromPartSize (Bytes n) = n

instance Sem.Semigroup PartSize where
	MegaBytes a <> MegaBytes b = MegaBytes (a + b)
	Bytes a <> b = Bytes (a + fromPartSize b)
	a <> Bytes b = Bytes (b + fromPartSize a)

instance Monoid PartSize where
	mempty = MegaBytes 0
	mappend = (Sem.<>)

reducePartSize :: PartSize -> PartSize -> PartSize
reducePartSize (MegaBytes a) (MegaBytes b) = MegaBytes (a - b)
reducePartSize (Bytes a) b = Bytes (a - fromPartSize b)
reducePartSize a (Bytes b) = Bytes (fromPartSize a - b)

-- | Partitions need to be aligned for optimal efficiency.
-- The alignment is a number of bytes.
newtype Alignment = Alignment ByteSize
	deriving (Show, Eq, Ord)

-- | 4MiB alignment is optimal for inexpensive flash drives and
-- is a good safe default for all drives.
safeAlignment :: Alignment
safeAlignment = Alignment (4*1024*1024)

fromAlignment :: Alignment -> ByteSize
fromAlignment (Alignment n) = n

-- | Flags that can be set on a partition.
data PartFlag = BootFlag | RootFlag | SwapFlag | HiddenFlag | RaidFlag | LvmFlag | LbaFlag | LegacyBootFlag | IrstFlag | EspFlag | PaloFlag | BiosGrubFlag
	deriving (Show)

instance PartedVal PartFlag where
	pval BootFlag = "boot"
	pval RootFlag = "root"
	pval SwapFlag = "swap"
	pval HiddenFlag = "hidden"
	pval RaidFlag = "raid"
	pval LvmFlag = "lvm"
	pval LbaFlag = "lba"
	pval LegacyBootFlag = "legacy_boot"
	pval IrstFlag = "irst"
	pval EspFlag = "esp"
	pval PaloFlag = "palo"
	pval BiosGrubFlag = "bios_grub"

instance PartedVal Bool where
	pval True = "on"
	pval False = "off"

-- This is used for creating partitions, not formatting partitions,
-- so it's ok to use eg, fat32 for both FAT and VFAT.
instance PartedVal Partition.Fs where
	pval Partition.EXT2 = "ext2"
	pval Partition.EXT3 = "ext3"
	pval Partition.EXT4 = "ext4"
	pval Partition.BTRFS = "btrfs"
	pval Partition.REISERFS = "reiserfs"
	pval Partition.XFS = "xfs"
	pval Partition.FAT = "fat32"
	pval Partition.VFAT = "fat32"
	pval Partition.NTFS = "ntfs"
	pval Partition.LinuxSwap = "linux-swap"
