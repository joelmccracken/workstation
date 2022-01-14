{-# LANGUAGE DeriveDataTypeable #-}
module Propellor.Types.Exception where

import Data.Typeable
import Control.Exception

-- | Normally when an exception is encountered while propellor is
-- ensuring a property, the property fails, but propellor robustly
-- continues on to the next property.
--
-- This is the only exception that will stop the entire propellor run,
-- preventing any subsequent properties of the Host from being ensured.
-- (When propellor is running in a container in a Host, this exception only
-- stops the propellor run in the container; the outer run in the Host
-- continues.)
--
-- You should only throw this exception when things are so badly messed up
-- that it's best for propellor to not try to do anything else.
data StopPropellorException = StopPropellorException String
	deriving (Show, Typeable)

instance Exception StopPropellorException
