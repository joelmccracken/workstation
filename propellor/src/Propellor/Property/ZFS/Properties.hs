-- | Maintainer: 2016 Evan Cofsky <evan@theunixman.com>
-- 
-- Functions defining zfs Properties.

module Propellor.Property.ZFS.Properties (
	ZFSOS,
	zfsExists,
	zfsSetProperties
) where

import Propellor.Base
import Data.List (intercalate)
import qualified Propellor.Property.ZFS.Process as ZP

-- | OS's that support ZFS
type ZFSOS = Linux + FreeBSD

-- | Will ensure that a ZFS volume exists with the specified mount point.
-- This requires the pool to exist as well, but we don't create pools yet.
zfsExists :: ZFS -> Property ZFSOS
zfsExists z = check (not <$> ZP.zfsExists z) create
	`describe` unwords ["Creating", zfsName z]
  where
	(p, a) = ZP.zfsCommand "create" [Nothing] z
	create = cmdProperty p a

-- | Sets the given properties. Returns True if all were successfully changed, False if not.
zfsSetProperties :: ZFS -> ZFSProperties -> Property ZFSOS
zfsSetProperties z setProperties = setall
	`requires` zfsExists z
  where
	spcmd :: String -> String -> (String, [String])
	spcmd p v = ZP.zfsCommand "set" [Just (intercalate "=" [p, v]), Nothing] z

	setprop :: (String, String) -> Property ZFSOS
	setprop (p, v) = check (ZP.zfsExists z) $
		cmdProperty (fst (spcmd p v)) (snd (spcmd p v))

	setall = combineProperties (unwords ["Setting properties on", zfsName z]) $
		toProps $ map setprop $ toPropertyList setProperties
