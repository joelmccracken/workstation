{-# LANGUAGE ConstrainedClassMethods #-}
-- | Types for ZFS Properties.
--
-- Copyright 2016 Evan Cofsky <evan@theunixman.com>
-- License: BSD 2-clause

module Propellor.Types.ZFS where

import Propellor.Types.ConfigurableValue
import Utility.Split

import Data.String
import qualified Data.Set as Set
import Data.List

-- | A single ZFS filesystem.
data ZFS = ZFS ZPool ZDataset deriving (Show, Eq, Ord)

-- | Represents a zpool.
data ZPool = ZPool String deriving (Show, Eq, Ord)

-- | Represents a dataset in a zpool.
--
-- Can be constructed from a / separated string.
data ZDataset = ZDataset [String] deriving (Eq, Ord)

type ZFSProperties = Set.Set ZFSProperty

fromList :: [ZFSProperty] -> ZFSProperties
fromList = Set.fromList

toPropertyList :: ZFSProperties -> [(String, String)]
toPropertyList = Set.foldr (\p l -> l ++ [toPair p]) []

fromPropertyList :: [(String, String)] -> ZFSProperties
fromPropertyList props =
	Set.fromList $ map fromPair props

zfsName :: ZFS -> String
zfsName (ZFS (ZPool pool) dataset) = intercalate "/" [pool, show dataset]

instance ConfigurableValue ZDataset where
	val (ZDataset paths) = intercalate "/" paths

instance Show ZDataset where
	show = val

instance IsString ZDataset where
	fromString s = ZDataset $ splitc '/' s

instance IsString ZPool where
	fromString p = ZPool p

class Value a where
	toValue :: a -> String
	fromValue :: (IsString a) => String -> a
	fromValue = fromString

data ZFSYesNo = ZFSYesNo Bool deriving (Show, Eq, Ord)
data ZFSOnOff = ZFSOnOff Bool deriving (Show, Eq, Ord)
data ZFSSize = ZFSSize Integer deriving (Show, Eq, Ord)
data ZFSString = ZFSString String deriving (Show, Eq, Ord)

instance Value ZFSYesNo where
	toValue (ZFSYesNo True) = "yes"
	toValue (ZFSYesNo False) = "no"

instance Value ZFSOnOff where
	toValue (ZFSOnOff True) = "on"
	toValue (ZFSOnOff False) = "off"

instance Value ZFSSize where
	toValue (ZFSSize s) = show s

instance Value ZFSString where
	toValue (ZFSString s) = s

instance IsString ZFSString where
	fromString = ZFSString

instance IsString ZFSYesNo where
	fromString "yes" = ZFSYesNo True
	fromString "no" = ZFSYesNo False
	fromString _ = error "Not yes or no"

instance IsString ZFSOnOff where
	fromString "on" = ZFSOnOff True
	fromString "off" = ZFSOnOff False
	fromString _ = error "Not on or off"

data ZFSACLInherit = AIDiscard | AINoAllow | AISecure | AIPassthrough deriving (Show, Eq, Ord)
instance IsString ZFSACLInherit where
	fromString "discard" = AIDiscard
	fromString "noallow" = AINoAllow
	fromString "secure" = AISecure
	fromString "passthrough" = AIPassthrough
	fromString _ = error "Not valid aclpassthrough value"

instance Value ZFSACLInherit where
	toValue AIDiscard = "discard"
	toValue AINoAllow = "noallow"
	toValue AISecure = "secure"
	toValue AIPassthrough = "passthrough"

data ZFSACLMode = AMDiscard | AMGroupmask | AMPassthrough deriving (Show, Eq, Ord)
instance IsString ZFSACLMode where
	fromString "discard" = AMDiscard
	fromString "groupmask" = AMGroupmask
	fromString "passthrough" = AMPassthrough
	fromString _ = error "Invalid zfsaclmode"

instance Value ZFSACLMode where
	toValue AMDiscard = "discard"
	toValue AMGroupmask = "groupmask"
	toValue AMPassthrough = "passthrough"

data ZFSProperty = Mounted ZFSYesNo
	       | Mountpoint ZFSString
	       | ReadOnly ZFSYesNo
	       | ACLInherit ZFSACLInherit
	       | ACLMode ZFSACLMode
	       | StringProperty String ZFSString
	       deriving (Show, Eq, Ord)

toPair :: ZFSProperty -> (String, String)
toPair (Mounted v) = ("mounted", toValue v)
toPair (Mountpoint v) = ("mountpoint", toValue v)
toPair (ReadOnly v) = ("readonly", toValue v)
toPair (ACLInherit v) = ("aclinherit", toValue v)
toPair (ACLMode v) = ("aclmode", toValue v)
toPair (StringProperty s v) = (s, toValue v)

fromPair :: (String, String) -> ZFSProperty
fromPair ("mounted", v) = Mounted (fromString v)
fromPair ("mountpoint", v) = Mountpoint (fromString v)
fromPair ("readonly", v) = ReadOnly (fromString v)
fromPair ("aclinherit", v) = ACLInherit (fromString v)
fromPair ("aclmode", v) = ACLMode (fromString v)
fromPair (s, v) = StringProperty s (fromString v)
