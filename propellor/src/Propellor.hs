{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | When propellor runs on a Host, it ensures that its Properties
-- are satisfied, taking action as necessary when a Property is not
-- currently satisfied.
--
-- A simple propellor program example:
--
-- > import Propellor
-- > import qualified Propellor.Property.File as File
-- > import qualified Propellor.Property.Apt as Apt
-- > 
-- > main :: IO ()
-- > main = defaultMain hosts
-- > 
-- > hosts :: [Host]
-- > hosts = [example]
-- > 
-- > example :: Host
-- > example = host "example.com" $ props
-- >     & Apt.installed ["mydaemon"]
-- >     & "/etc/mydaemon.conf" `File.containsLine` "secure=1"
-- >       `onChange` cmdProperty "service" ["mydaemon", "restart"]
-- >     ! Apt.installed ["unwantedpackage"]
--
-- See config.hs for a more complete example, and clone Propellor's
-- git repository for a deployable system using Propellor:
-- git clone <git://git.joeyh.name/propellor>

module Propellor (
	-- * Core data types
	  Host(..)
	, Property
	, RevertableProperty
	, module Propellor.Types
	-- * Config file
	, defaultMain
	, host
	, (&)
	, (!)
	-- * Propertries
	-- | Properties are often combined together in your propellor
	-- configuration. For example:
	--
	-- > "/etc/foo/config" `File.containsLine` "bar=1"
	-- > 	`requires` File.dirExists "/etc/foo"
	, requires
	, before
	, onChange
	, describe
	, module Propellor.Property
	-- | Everything you need to build your own properties,
	-- and useful property combinators
	, module Propellor.Property.Cmd
	-- | Properties to run shell commands
	, module Propellor.Info
	-- | Properties that set `Info`
	, module Propellor.Property.List
	-- | Combining a list of properties into a single property
	, module Propellor.Types.PrivData
	-- | Private data access for properties

	, module X
) where

import Propellor.Types
import Propellor.CmdLine (defaultMain)
import Propellor.Property
import Propellor.Property.List
import Propellor.Property.Cmd
import Propellor.Types.PrivData
import Propellor.Info
import Propellor.PropAccum

import Data.Monoid as X
import Data.String as X (fromString)
