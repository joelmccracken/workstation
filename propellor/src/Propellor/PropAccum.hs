{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Propellor.PropAccum
	( host
	, Props(..)
	, props
	, (&)
	, (&^)
	, (!)
	) where

import Propellor.Types
import Propellor.Types.MetaTypes
import Propellor.Types.Core
import Propellor.Property

import GHC.TypeLits
import Data.Monoid
import Prelude

-- | Defines a host and its properties.
--
-- > host "example.com" $ props
-- > 	& someproperty
-- > 	! oldproperty
-- > 	& otherproperty
host :: HostName -> Props metatypes -> Host
host hn (Props ps) = Host hn ps (mconcat (map getInfoRecursive ps))

-- | Start accumulating a list of properties.
--
-- Properties can be added to it using `(&)` etc.
props :: Props UnixLike
props = Props []

infixl 1 &
infixl 1 &^
infixl 1 !

type family GetMetaTypes x where
	GetMetaTypes (Property (MetaTypes t)) = MetaTypes t
	GetMetaTypes (RevertableProperty (MetaTypes t) undo) = MetaTypes t

-- When many properties are combined, ghc error message
-- can include quite a lot of code, typically starting with
-- `props` and including all the properties up to and including the
-- one that fails to combine. Point the user in the right direction.
type family NoteFor symbol :: ErrorMessage where
	NoteFor symbol =
		'Text "Probably the problem is with the last property added with "
			':<>: symbol
			':<>: 'Text " in the code excerpt below."

-- | Adds a property to a Props.
--
-- Can add Properties and RevertableProperties
(&)
	::
		( IsProp p
		-- -Wredundant-constraints is turned off because
		-- this constraint appears redundant, but is actually
		-- crucial.
		, MetaTypes y ~ GetMetaTypes p
		, CheckCombinableNote x y (NoteFor ('Text "&"))
		)
	=> Props (MetaTypes x)
	-> p
	-> Props (MetaTypes (Combine x y))
Props c & p = Props (c ++ [toChildProperty p])

-- | Adds a property before any other properties.
(&^)
	::
		( IsProp p
		-- -Wredundant-constraints is turned off because
		-- this constraint appears redundant, but is actually
		-- crucial.
		, MetaTypes y ~ GetMetaTypes p
		, CheckCombinableNote x y (NoteFor ('Text "&^"))
		)
	=> Props (MetaTypes x)
	-> p
	-> Props (MetaTypes (Combine x y))
Props c &^ p = Props (toChildProperty p : c)

-- | Adds a property in reverted form.
(!)
	-- -Wredundant-constraints is turned off because
	-- this constraint appears redundant, but is actually
	-- crucial.
	:: CheckCombinableNote x z (NoteFor ('Text "!"))
	=> Props (MetaTypes x)
	-> RevertableProperty (MetaTypes y) (MetaTypes z)
	-> Props (MetaTypes (Combine x z))
Props c ! p = Props (c ++ [toChildProperty (revert p)])
