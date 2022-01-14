{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types (
	-- * Core data types
	  Host(..)
	, Property(..)
	, property
	, property''
	, Desc
	, RevertableProperty(..)
	, (<!>)
	, Propellor(..)
	, LiftPropellor(..)
	, Info
	-- * Types of properties
	, UnixLike
	, Linux
	, DebianLike
	, Debian
	, Buntish
	, ArchLinux
	, FreeBSD
	, HasInfo
	, type (+)
	, TightenTargets(..)
	, TightenTargetsAllowed
	-- * Combining and modifying properties
	, Combines(..)
	, CombinedType
	, ResultCombiner
	, adjustPropertySatisfy
	-- * Other included types
	, module Propellor.Types.OS
	, module Propellor.Types.ConfigurableValue
	, module Propellor.Types.Dns
	, module Propellor.Types.Result
	, module Propellor.Types.ZFS
	) where

import GHC.TypeLits hiding (type (+))
import GHC.Exts (Constraint)
import Data.Type.Bool
import qualified Data.Semigroup as Sem
import Data.Monoid
import Control.Applicative
import Prelude

import Propellor.Types.Core
import Propellor.Types.Info
import Propellor.Types.OS
import Propellor.Types.ConfigurableValue
import Propellor.Types.Dns
import Propellor.Types.Result
import Propellor.Types.MetaTypes
import Propellor.Types.ZFS

-- | The core data type of Propellor, this represents a property
-- that the system should have, with a description, and an action to ensure
-- it has the property.
--
-- There are different types of properties that target different OS's,
-- and so have different metatypes. 
-- For example: "Property DebianLike" and "Property FreeBSD".
--
-- Also, some properties have associated `Info`, which is indicated in
-- their type: "Property (HasInfo + DebianLike)"
--
-- There are many associated type families, which are mostly used
-- internally, so you needn't worry about them.
data Property metatypes = Property metatypes Desc (Maybe (Propellor Result)) Info [ChildProperty]

instance Show (Property metatypes) where
	show p = "property " ++ show (getDesc p)

-- | Constructs a Property, from a description and an action to run to
-- ensure the Property is met.
--
-- Due to the polymorphic return type of this function, most uses will need
-- to specify a type signature. This lets you specify what OS the property
-- targets, etc.
--
-- For example:
--
-- > foo :: Property Debian
-- > foo = property "foo" $ do
-- >	...
-- > 	return MadeChange
property
	:: SingI metatypes
	=> Desc
	-> Propellor Result
	-> Property (MetaTypes metatypes)
property d a = Property sing d (Just a) mempty mempty

property''
	:: SingI metatypes
	=> Desc
	-> Maybe (Propellor Result)
	-> Property (MetaTypes metatypes)
property'' d a = Property sing d a mempty mempty

-- | Changes the action that is performed to satisfy a property.
adjustPropertySatisfy :: Property metatypes -> (Propellor Result -> Propellor Result) -> Property metatypes
adjustPropertySatisfy (Property t d s i c) f = Property t d (f <$> s) i c

-- | A property that can be reverted. The first Property is run
-- normally and the second is run when it's reverted.
--
-- See `Propellor.Property.Versioned.Versioned` 
-- for a way to use RevertableProperty to define different
-- versions of a host.
data RevertableProperty setupmetatypes undometatypes = RevertableProperty
	{ setupRevertableProperty :: Property setupmetatypes
	, undoRevertableProperty :: Property undometatypes
	}

instance Show (RevertableProperty setupmetatypes undometatypes) where
	show (RevertableProperty p _) = show p

-- | Shorthand to construct a revertable property from any two Properties.
(<!>)
	:: Property setupmetatypes
	-> Property undometatypes
	-> RevertableProperty setupmetatypes undometatypes
setup <!> undo = RevertableProperty setup undo

instance IsProp (Property metatypes) where
	setDesc (Property t _ a i c) d = Property t d a i c
	getDesc (Property _ d _ _ _) = d
	getChildren (Property _ _ _ _ c) = c
	addChildren (Property t d a i c) c' = Property t d a i (c ++ c')
	getInfoRecursive (Property _ _ _ i c) =
		i <> mconcat (map getInfoRecursive c)
	getInfo (Property _ _ _ i _) = i
	toChildProperty (Property _ d a i c) = ChildProperty d a i c
	getSatisfy (Property _ _ a _ _) = a

instance IsProp (RevertableProperty setupmetatypes undometatypes) where
	-- | Sets the description of both sides.
	setDesc (RevertableProperty p1 p2) d =
		RevertableProperty (setDesc p1 d) (setDesc p2 ("not " ++ d))
	getDesc (RevertableProperty p1 _) = getDesc p1
	getChildren (RevertableProperty p1 _) = getChildren p1
	-- | Only add children to the active side.
	addChildren (RevertableProperty p1 p2) c = RevertableProperty (addChildren p1 c) p2
	-- | Return the Info of the currently active side.
	getInfoRecursive (RevertableProperty p1 _p2) = getInfoRecursive p1
	getInfo (RevertableProperty p1 _p2) = getInfo p1
	toChildProperty (RevertableProperty p1 _p2) = toChildProperty p1
	getSatisfy (RevertableProperty p1 _) = getSatisfy p1

-- | Type level calculation of the type that results from combining two
-- types of properties.
type family CombinedType x y where
	CombinedType (Property (MetaTypes x)) (Property (MetaTypes y)) =
		Property (MetaTypes (Combine x y))
	CombinedType
		(RevertableProperty (MetaTypes x) (MetaTypes x'))
		(RevertableProperty (MetaTypes y) (MetaTypes y')) =
			RevertableProperty (MetaTypes (Combine x y)) (MetaTypes (Combine x' y'))
	-- When only one of the properties is revertable, the combined
	-- property is not fully revertable, so is not a RevertableProperty.
	CombinedType (RevertableProperty (MetaTypes x) (MetaTypes x')) (Property (MetaTypes y)) =
		Property (MetaTypes (Combine x y))
	CombinedType (Property (MetaTypes x)) (RevertableProperty (MetaTypes y) (MetaTypes y')) =
		Property (MetaTypes (Combine x y))

type ResultCombiner = Maybe (Propellor Result) -> Maybe (Propellor Result) -> Maybe (Propellor Result)

class Combines x y where
	-- | Combines together two properties, yielding a property that
	-- has the description and info of the first, and that has the
	-- second property as a child property.
	combineWith
		:: ResultCombiner
		-- ^ How to combine the actions to satisfy the properties.
		-> ResultCombiner
		-- ^ Used when combining revertable properties, to combine
		-- their reversion actions.
		-> x
		-> y
		-> CombinedType x y

instance (CheckCombinable x y, SingI (Combine x y)) => Combines (Property (MetaTypes x)) (Property (MetaTypes y)) where
	combineWith f _ (Property _ d1 a1 i1 c1) (Property _ d2 a2 i2 c2) =
		Property sing d1 (f a1 a2) i1 (ChildProperty d2 a2 i2 c2 : c1)
instance (CheckCombinable x y, CheckCombinable x' y', SingI (Combine x y), SingI (Combine x' y')) => Combines (RevertableProperty (MetaTypes x) (MetaTypes x')) (RevertableProperty (MetaTypes y) (MetaTypes y')) where
	combineWith sf tf (RevertableProperty s1 t1) (RevertableProperty s2 t2) =
		RevertableProperty
			(combineWith sf tf s1 s2)
			(combineWith tf sf t1 t2)
instance (CheckCombinable x y, SingI (Combine x y)) => Combines (RevertableProperty (MetaTypes x) (MetaTypes x')) (Property (MetaTypes y)) where
	combineWith sf tf (RevertableProperty x _) y = combineWith sf tf x y
instance (CheckCombinable x y, SingI (Combine x y)) => Combines (Property (MetaTypes x)) (RevertableProperty (MetaTypes y) (MetaTypes y')) where
	combineWith sf tf x (RevertableProperty y _) = combineWith sf tf x y

class TightenTargets p where
	-- | Tightens the MetaType list of a Property (or similar),
	-- to contain fewer targets.
	--
	-- For example, to make a property that uses apt-get, which is only
	-- available on DebianLike systems:
	--
	-- > upgraded :: Property DebianLike
	-- > upgraded = tightenTargets $ cmdProperty "apt-get" ["upgrade"]
	tightenTargets
		:: 
			( TightenTargetsAllowed untightened tightened
			, SingI tightened
			)
		=> p (MetaTypes untightened)
		-> p (MetaTypes tightened)

-- Note that this uses PolyKinds
type family TightenTargetsAllowed untightened tightened :: Constraint where
	TightenTargetsAllowed untightened tightened =
		If (Targets tightened `IsSubset` Targets untightened
		    && NonTargets untightened `IsSubset` NonTargets tightened)
			('True ~ 'True)
			(IfStuck (Targets tightened)
				(DelayError
					('Text "Unable to infer desired Property type in this use of tightenTargets."
					 ':$$: ('Text "Consider adding a type annotation.")
					)
				)
				(DelayErrorFcf
					('Text "This use of tightenTargets would widen, not narrow, adding: "
					 ':$$: PrettyPrintMetaTypes (Difference (Targets tightened) (Targets untightened))
					)
				)
			)

instance TightenTargets Property where
	tightenTargets (Property _ d a i c) = Property sing d a i c

-- | Any type of Property is a Semigroup. When properties x and y are
-- appended together, the resulting property has a description like
-- "x and y". Note that when x fails to be ensured, it will not
-- try to ensure y.
instance SingI metatypes => Sem.Semigroup (Property (MetaTypes metatypes))
  where
	Property _ d1 a1 i1 c1 <> Property _ d2 a2 i2 c2 =
	  	Property sing d (a1 <> a2) (i1 <> i2) (c1 <> c2)
	  where
		-- Avoid including "noop property" in description
		-- when using eg mconcat.
		d = case (a1, a2) of
			(Just _, Just _) -> d1 <> " and " <> d2
			(Just _, Nothing) -> d1
			(Nothing, Just _) -> d2
			(Nothing, Nothing) -> d1

-- | Any type of Property is a Monoid.
instance SingI metatypes => Monoid (Property (MetaTypes metatypes))
  where
	-- | A property that does nothing.
	mempty = Property sing "noop property" Nothing mempty mempty
	mappend = (Sem.<>)

-- | Any type of RevertableProperty is a Semigroup. When revertable 
-- properties x and y are appended together, the resulting revertable
-- property has a description like "x and y".
-- Note that when x fails to be ensured, it will not try to ensure y.
instance
	( Sem.Semigroup (Property (MetaTypes setupmetatypes))
	, Sem.Semigroup (Property (MetaTypes undometatypes))
	, SingI setupmetatypes
	, SingI undometatypes
	)
	=> Sem.Semigroup (RevertableProperty (MetaTypes setupmetatypes) (MetaTypes undometatypes))
  where
	RevertableProperty s1 u1 <> RevertableProperty s2 u2 =
		RevertableProperty (s1 <> s2) (u2 <> u1)

instance
	( Monoid (Property (MetaTypes setupmetatypes))
	, Monoid (Property (MetaTypes undometatypes))
	, SingI setupmetatypes
	, SingI undometatypes
	)
	=> Monoid (RevertableProperty (MetaTypes setupmetatypes) (MetaTypes undometatypes))
  where
	mempty = RevertableProperty mempty mempty
	mappend = (Sem.<>)
