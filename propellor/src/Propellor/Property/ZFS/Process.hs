-- | Maintainer: 2016 Evan Cofsky <evan@theunixman.com>
-- 
-- Functions running zfs processes.

module Propellor.Property.ZFS.Process where

import Propellor.Base
import Utility.Split

import Data.List

-- | Gets the properties of a ZFS volume.
zfsGetProperties ::  ZFS -> IO ZFSProperties
zfsGetProperties z =
	let plist = fromPropertyList . map (\(_:k:v:_) -> (k, v)) . (map (split "\t"))
	in plist <$> runZfs "get" [Just "-H", Just "-p", Just "all"] z

zfsExists :: ZFS -> IO Bool
zfsExists z = any id . map (isInfixOf (zfsName z))
	<$> runZfs "list" [Just "-H"] z

-- | Runs the zfs command with the arguments.
--
-- Runs the command with -H which will skip the header line and
-- separate all fields with tabs.
--
-- Replaces Nothing in the argument list with the ZFS pool/dataset.
runZfs :: String -> [Maybe String] -> ZFS -> IO [String]
runZfs cmd args z = lines <$> uncurry readProcess (zfsCommand cmd args z)

-- | Return the ZFS command line suitable for readProcess or cmdProperty.
zfsCommand :: String -> [Maybe String] -> ZFS -> (String, [String])
zfsCommand cmd args z = ("zfs", cmd:(map (maybe (zfsName z) id) args))
