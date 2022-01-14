module Propellor.Types.PartSpec where

import Propellor.Property.Parted.Types
import Propellor.Property.Mount

-- | Specifies a mount point, mount options, and a constructor for a
-- Partition that determines its size.
type PartSpec t = (Maybe MountPoint, MountOpts, PartSize -> Partition, t)

-- | Specifies a partition table.
data PartTableSpec = PartTableSpec TableType [PartSpec ()]

instance Show PartTableSpec where
	show (PartTableSpec tt _) = "PartTableSpec " ++ show tt
