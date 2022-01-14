-- | Maintainer: 2016 Evan Cofsky <evan@theunixman.com>
-- 
-- FreeBSD Properties
--
-- This module is designed to be imported unqualified.

module Propellor.Property.FreeBSD (
	module Propellor.Property.FreeBSD.Pkg,
	module Propellor.Property.FreeBSD.Poudriere
) where

import Propellor.Property.FreeBSD.Pkg
import Propellor.Property.FreeBSD.Poudriere
