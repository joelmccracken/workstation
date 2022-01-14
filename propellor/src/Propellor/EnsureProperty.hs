{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Propellor.EnsureProperty
	( ensureProperty
	, property'
	, OuterMetaTypesWitness
	, EnsurePropertyAllowed
	) where

import Propellor.Types
import Propellor.Types.Core
import Propellor.Types.MetaTypes
import Propellor.Exception

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Type.Bool
import Data.Monoid
import Prelude

-- | For when code running in the Propellor monad needs to ensure a
-- Property.
--
-- Use `property'` to get the `OuterMetaTypesWithness`. For example:
--
-- > foo = Property Debian
-- > foo = property' "my property" $ \w -> do
-- > 	ensureProperty w (aptInstall "foo")
--
-- The type checker will prevent using ensureProperty with a property
-- that does not support the target OSes needed by the OuterMetaTypesWitness.
-- In the example above, aptInstall must support Debian, since foo
-- is supposed to support Debian.
--
-- The type checker will also prevent using ensureProperty with a property
-- with HasInfo in its MetaTypes. Doing so would cause the `Info` associated
-- with the property to be lost.
ensureProperty
	::
		-- -Wredundant-constraints is turned off because
		-- this constraint appears redundant, but is actually
		-- crucial.
		( EnsurePropertyAllowed inner outer)
	=> OuterMetaTypesWitness outer
	-> Property (MetaTypes inner)
	-> Propellor Result
ensureProperty _ = maybe (return NoChange) catchPropellor . getSatisfy

type family EnsurePropertyAllowed inner outer :: Constraint where
	EnsurePropertyAllowed inner outer = 'True ~
		((EnsurePropertyNoInfo inner)
			&&
		(EnsurePropertyTargetOSMatches inner outer))

type family EnsurePropertyNoInfo (l :: [a]) :: Bool where
	EnsurePropertyNoInfo '[] = 'True
	EnsurePropertyNoInfo (t ': ts) = If (Not (t `EqT` 'WithInfo))
		(EnsurePropertyNoInfo ts)
		(TypeError ('Text "Cannot use ensureProperty with a Property that HasInfo."))

type family EnsurePropertyTargetOSMatches inner outer where
	EnsurePropertyTargetOSMatches inner outer = 
		If (Targets outer `IsSubset` Targets inner)
			'True
			(IfStuck (Targets outer)
				(DelayError
					('Text "ensureProperty outer Property type is not able to be inferred here."
					 ':$$: 'Text "Consider adding a type annotation."
					)
				)
				(DelayErrorFcf
					('Text "ensureProperty inner Property is missing support for: "
					 ':$$: PrettyPrintMetaTypes (Difference (Targets outer) (Targets inner))
					)
				)
			)

-- | Constructs a property, like `property`, but provides its
-- `OuterMetaTypesWitness`.
property'
	:: SingI metatypes
	=> Desc
	-> (OuterMetaTypesWitness metatypes -> Propellor Result)
	-> Property (MetaTypes metatypes)
property' d a =
	let p = Property sing d (Just (a (outerMetaTypesWitness p))) mempty mempty
	in p

-- | Used to provide the metatypes of a Property to calls to 
-- 'ensureProperty` within it.
newtype OuterMetaTypesWitness metatypes = OuterMetaTypesWitness (MetaTypes metatypes)

outerMetaTypesWitness :: Property (MetaTypes l) -> OuterMetaTypesWitness l
outerMetaTypesWitness (Property metatypes _ _ _ _) = OuterMetaTypesWitness metatypes
