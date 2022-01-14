{-# LANGUAGE RankNTypes, FlexibleContexts, TypeFamilies #-}

-- | Versioned properties and hosts.
--
-- When importing and using this module, you will need to enable some
-- language extensions:
--
-- > {-# LANGUAGE RankNTypes, FlexibleContexts, TypeFamilies #-}
--
-- This module takes advantage of `RevertableProperty` to let propellor
-- switch cleanly between versions. The way it works is all revertable
-- properties for other versions than the current version are first
-- reverted, and  then propellor ensures the property for the current
-- version. This method should work for any combination of revertable
-- properties.
--
-- For example:
-- 
-- > demo :: Versioned Int (RevertableProperty DebianLike DebianLike)
-- > demo ver =
-- > 	ver (   (== 1) --> Apache.modEnabled "foo"
-- >		`requires` Apache.modEnabled "foosupport"
-- >	    <|> (== 2) --> Apache.modEnabled "bar"
-- > 	    <|> (> 2)  --> Apache.modEnabled "baz"
-- > 	    )
-- >
-- > foo :: Host
-- > foo = host "foo.example.com" $ props
-- > 	& demo `version` (2 :: Int)
--
-- Similarly, a whole Host can be versioned. For example:
--
-- > bar :: Versioned Int Host
-- > bar ver = host "bar.example.com" $ props
-- >	& osDebian Unstable X86_64
-- > 	& ver (   (== 1) --> Apache.modEnabled "foo"
-- > 	      <|> (== 2) --> Apache.modEnabled "bar"
-- > 	      )
-- > 	& ver ( (>= 2) --> Apt.unattendedUpgrades )
--
-- Note that some versioning of revertable properties may cause
-- propellor to do a lot of unnecessary work each time it's run.
-- Here's an example of such a problem:
--
-- > slow :: Versioned Int -> RevertableProperty DebianLike DebianLike
-- > slow ver =
-- > 	ver (   (== 1) --> (Apt.installed "foo" <!> Apt.removed "foo")
-- >	    <|> (== 2) --> (Apt.installed "bar" <!> Apt.removed "bar")
-- >        )
--
-- Suppose that package bar depends on package foo. Then at version 2,
-- propellor will remove package foo in order to revert version 1, only
-- to re-install it since version 2 also needs it installed.

module Propellor.Property.Versioned (Versioned, version, (-->), (<|>)) where

import Propellor
import Propellor.Types.Core

import Data.List

-- | Something that has multiple versions of type `v`.
type Versioned v t = VersionedBy v -> t

type VersionedBy v
	= forall metatypes. Combines (RevertableProperty metatypes metatypes) (RevertableProperty metatypes metatypes)
	=> (CombinedType (RevertableProperty metatypes metatypes) (RevertableProperty metatypes metatypes) ~ RevertableProperty metatypes metatypes)
	=> (VerSpec v metatypes -> RevertableProperty metatypes metatypes)

-- | Access a particular version of a Versioned value.
version :: (Versioned v t) -> v -> t
version f v = f (processVerSpec v)

-- A specification of versions.
--
-- Why is this not a simple list like
-- [(v -> Bool, RevertableProperty metatypes metatypes)] ?
-- Using a list would mean the empty list would need to be dealt with,
-- and processVerSpec does not have a Monoid instance for
-- RevertableProperty metatypes metatypes in scope, and due to the way the
-- Versioned type works, the compiler cannot find such an instance.
--
-- Also, using this data type allows a nice syntax for creating
-- VerSpecs, via the `<&>` and `alt` functions.
data VerSpec v metatypes
	= Base (v -> Bool, RevertableProperty metatypes metatypes)
	| More (v -> Bool, RevertableProperty metatypes metatypes) (VerSpec v metatypes)

processVerSpec 
	:: Combines (RevertableProperty metatypes metatypes) (RevertableProperty metatypes metatypes)
	=> (CombinedType (RevertableProperty metatypes metatypes) (RevertableProperty metatypes metatypes) ~ RevertableProperty metatypes metatypes)
	=> v
	-> VerSpec v metatypes
	-> RevertableProperty metatypes metatypes
processVerSpec v s = combinedp s
	`describe` intercalate " and " (combineddesc s [])
  where
	combinedp (Base (c, p))
		| c v = p
		| otherwise = revert p
	combinedp (More (c, p) vs)
		| c v = combinedp vs `before` p
		| otherwise = revert p `before` combinedp vs
	combineddesc (Base (c, p)) l
		| c v = getDesc p : l
		| otherwise = getDesc (revert p) : l
	combineddesc (More (c, p) vs) l
		| c v = getDesc p : combineddesc vs l
		| otherwise = getDesc (revert p) : combineddesc vs l

-- | Specify a function that checks the version, and what
-- `RevertableProperty` to use if the version matches.
(-->) :: (v -> Bool) -> RevertableProperty metatypes metatypes -> VerSpec v metatypes
c --> p = Base (c, p)

-- | Add an alternate version.
(<|>) :: VerSpec v metatypes -> VerSpec v metatypes -> VerSpec v metatypes 
Base a <|> Base b = More a (Base b)
Base a <|> More b c = More a (More b c)
More b c <|> Base a  = More a (More b c)
More a b <|> More c d = More a (More c (b <|> d))

infixl 8 -->
infixl 2 <|>
