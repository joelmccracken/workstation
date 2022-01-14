{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- | Disk image partition specification.

module Propellor.Property.DiskImage.PartSpec (
	PartSpec,
	Fs(..),
	PartSize(..),
	partition,
	-- * PartSpec combinators
	swapPartition,
	rawPartition,
	mountedAt,
	addFreeSpace,
	setSize,
	mountOpt,
	errorReadonly,
	reservedSpacePercentage,
	setFlag,
	extended,
	-- * Partition properties
	--
	-- | These properties do not do any disk partitioning on their own, but
	-- the Info they set can be used when building a disk image for a
	-- host.
	hasPartition,
	adjustPartition,
	PartLocation(..),
	partLocation,
	hasPartitionTableType,
	TableType(..),
	PartInfo,
	toPartTableSpec,
	PartTableSpec(..)
) where

import Propellor.Base
import Propellor.Property.Parted
import Propellor.Types.PartSpec
import Propellor.Types.Info
import Propellor.Property.Mount

import Data.List (sortBy)
import Data.Ord
import qualified Data.Semigroup as Sem

-- | Specifies a partition with a given filesystem.
--
-- The partition is not mounted anywhere by default; use the combinators
-- below to configure it.
partition :: Monoid t => Fs -> PartSpec t
partition fs = (Nothing, mempty, mkPartition (Just fs), mempty)

-- | Specifies a swap partition of a given size.
swapPartition :: Monoid t => PartSize -> PartSpec t
swapPartition sz = (Nothing, mempty, const (mkPartition (Just LinuxSwap) sz), mempty)

-- | Specifies a partition without any filesystem, of a given size.
rawPartition :: Monoid t => PartSize -> PartSpec t
rawPartition sz = (Nothing, mempty, const (mkPartition Nothing sz), mempty)

-- | Specifies where to mount a partition.
mountedAt :: PartSpec t -> MountPoint -> PartSpec t
mountedAt (_, o, p, t) mp = (Just mp, o, p, t)

-- | Partitions in disk images default to being sized large enough to hold
-- the files that live in that partition.
--
-- This adds additional free space to a partition.
addFreeSpace :: PartSpec t -> PartSize -> PartSpec t
addFreeSpace (mp, o, p, t) freesz = (mp, o, p', t)
  where
	p' = \sz -> p (sz <> freesz)

-- | Specify a fixed size for a partition.
setSize :: PartSpec t -> PartSize -> PartSpec t
setSize (mp, o, p, t) sz = (mp, o, const (p sz), t)

-- | Specifies a mount option, such as "noexec"
mountOpt :: ToMountOpts o => PartSpec t -> o -> PartSpec t
mountOpt (mp, o, p, t) o' = (mp, o <> toMountOpts o', p, t)

-- | Mount option to make a partition be remounted readonly when there's an
-- error accessing it.
errorReadonly :: MountOpts
errorReadonly = toMountOpts "errors=remount-ro"

-- | Sets the percent of the filesystem blocks reserved for the super-user.
--
-- The default is 5% for ext2 and ext4. Some filesystems may not support
-- this.
reservedSpacePercentage :: PartSpec t -> Int -> PartSpec t
reservedSpacePercentage s percent = adjustp s $ \p -> 
	p { partMkFsOpts = ("-m"):show percent:partMkFsOpts p }

-- | Sets a flag on the partition.
setFlag :: PartSpec t -> PartFlag -> PartSpec t
setFlag s f = adjustp s $ \p -> p { partFlags = (f, True):partFlags p }

-- | Makes a MSDOS partition be Extended, rather than Primary.
extended :: PartSpec t -> PartSpec t
extended s = adjustp s $ \p -> p { partType = Extended }

adjustp :: PartSpec t -> (Partition -> Partition) -> PartSpec t
adjustp (mp, o, p, t) f = (mp, o, f . p, t)

data PartInfoVal
	= TableTypeInfo TableType
	| PartSpecInfo (PartSpec PartLocation)
	| AdjustPartSpecInfo MountPoint (PartSpec PartLocation -> PartSpec PartLocation)

newtype PartInfo = PartInfo [PartInfoVal]
	deriving (Monoid, Sem.Semigroup, Typeable)

instance IsInfo PartInfo where
	propagateInfo _ = PropagateInfo False

instance Show PartInfo where
	show = show . toPartTableSpec

toPartTableSpec :: PartInfo -> PartTableSpec
toPartTableSpec (PartInfo l) = PartTableSpec tt pil
  where
	tt = fromMaybe MSDOS $ headMaybe $ reverse $ mapMaybe gettt l

	pil = map convert $ sortBy (comparing location) $ adjust collect
	collect = mapMaybe getspartspec l
	adjust ps = adjust' ps (mapMaybe getadjust l)
	adjust' ps [] = ps
	adjust' ps ((mp, f):rest) = adjust' (map (adjustone mp f) ps) rest
	adjustone mp f p@(mp', _, _, _)
		| Just mp == mp' = f p
		| otherwise = p
	location (_, _, _, loc) = loc
	convert (mp, o, p, _) = (mp, o, p, ())
	
	gettt (TableTypeInfo t) = Just t
	gettt _ = Nothing
	getspartspec (PartSpecInfo ps) = Just ps
	getspartspec _ = Nothing
	getadjust (AdjustPartSpecInfo mp f) = Just (mp, f)
	getadjust _ = Nothing

-- | Indicates the partition table type of a host.
--
-- When not specified, the default is MSDOS.
--
-- For example:
--
-- >	& hasPartitionTableType GPT
hasPartitionTableType :: TableType -> Property (HasInfo + UnixLike)
hasPartitionTableType tt = pureInfoProperty
	("partition table type " ++ show tt)
	(PartInfo [TableTypeInfo tt])

-- | Indicates that a host has a partition.
--
-- For example:
--
-- >	& hasPartiton (partition EXT2 `mountedAt` "/boot" `partLocation` Beginning)
-- >	& hasPartiton (partition EXT4 `mountedAt` "/")
-- >	& hasPartiton (partition EXT4 `mountedAt` "/home" `partLocation` End `reservedSpacePercentage` 0)
hasPartition :: PartSpec PartLocation -> Property (HasInfo + UnixLike)
hasPartition p@(mmp, _, _, _) = pureInfoProperty desc
	(PartInfo [PartSpecInfo p])
  where
	desc = case mmp of
		Just mp -> mp ++ " partition"
		Nothing -> "unmounted partition"

-- | Adjusts the PartSpec for the partition mounted at the specified location.
--
-- For example:
--
-- > 	& adjustPartition "/boot" (`addFreeSpace` MegaBytes 150)
adjustPartition :: MountPoint -> (PartSpec PartLocation -> PartSpec PartLocation) -> Property (HasInfo + UnixLike)
adjustPartition mp f = pureInfoProperty
	(mp ++ " adjusted")
	(PartInfo [AdjustPartSpecInfo mp f])

-- | Indicates partition layout in a disk. Default is somewhere in the
-- middle.
data PartLocation = Beginning | Middle | End
	deriving (Eq, Ord)

instance Sem.Semigroup PartLocation where
	_ <> b = b

instance Monoid PartLocation where
	mempty = Middle
	mappend = (Sem.<>)

partLocation :: PartSpec PartLocation -> PartLocation -> PartSpec PartLocation
partLocation (mp, o, p, _) l = (mp, o, p, l)
