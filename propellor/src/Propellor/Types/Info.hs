{-# LANGUAGE GADTs, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Propellor.Types.Info (
	Info(..),
	InfoEntry(..),
	IsInfo(..),
	PropagateInfo(..),
	addInfo,
	toInfo,
	fromInfo,
	mapInfo,
	InfoVal(..),
	fromInfoVal,
	Typeable,
) where

import Data.Dynamic
import Data.Maybe
import Data.Monoid
import qualified Data.Semigroup as Sem
import qualified Data.Typeable as T
import Prelude

-- | Information about a Host, which can be provided by its properties.
--
-- Many different types of data can be contained in the same Info value
-- at the same time. See `toInfo` and `fromInfo`.
newtype Info = Info [InfoEntry]
	deriving (Sem.Semigroup, Monoid, Show)

data InfoEntry where
	InfoEntry :: (IsInfo v, Typeable v) => v -> InfoEntry

instance Show InfoEntry where
	show (InfoEntry v) = show v

-- Extracts the value from an InfoEntry but only when
-- it's of the requested type.
extractInfoEntry :: Typeable v => InfoEntry -> Maybe v
extractInfoEntry (InfoEntry v) = T.cast v

-- | Values stored in Info must be members of this class.
--
-- This is used to avoid accidentially using other data types
-- as info, especially type aliases which coud easily lead to bugs.
-- We want a little bit of dynamic types here, but not too far..
class (Typeable v, Monoid v, Show v) => IsInfo v where
	-- | Should this info be propagated out of a container to its Host?
	propagateInfo :: v -> PropagateInfo

data PropagateInfo
	= PropagateInfo Bool
	| PropagatePrivData
	-- ^ Info about PrivData generally will be propigated even in cases
	-- where other Info is not, so it treated specially.

-- | Any value in the `IsInfo` type class can be added to an Info.
addInfo :: IsInfo v => Info -> v -> Info
addInfo (Info l) v = Info (l++[InfoEntry v])

-- | Converts any value in the `IsInfo` type class into an Info,
-- which is otherwise empty.
toInfo :: IsInfo v => v -> Info
toInfo = addInfo mempty

-- | Extracts a value from an Info.
fromInfo :: IsInfo v => Info -> v
fromInfo (Info l) = mconcat (mapMaybe extractInfoEntry l)

-- | Maps a function over all values stored in the Info that are of the
-- appropriate type.
mapInfo :: IsInfo v => (v -> v) -> Info -> Info
mapInfo f (Info l) = Info (map go l)
  where
	go i = case extractInfoEntry i of
		Nothing -> i
		Just v -> InfoEntry (f v)

-- | Use this to put a value in Info that is not a monoid.
-- The last value set will be used. This info does not propagate
-- out of a container.
data InfoVal v = NoInfoVal | InfoVal v
	deriving (Typeable, Show)

instance Sem.Semigroup (InfoVal v) where
	_ <> v@(InfoVal _) = v
	v <> NoInfoVal = v

instance Monoid (InfoVal v) where
	mempty = NoInfoVal
	mappend = (Sem.<>)

instance (Typeable v, Show v) => IsInfo (InfoVal v) where
	propagateInfo _ = PropagateInfo False

fromInfoVal :: InfoVal v -> Maybe v
fromInfoVal NoInfoVal = Nothing
fromInfoVal (InfoVal v) = Just v
