-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Timezone where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File

-- | A timezone from /usr/share/zoneinfo
type Timezone = String

-- | Sets the system's timezone
configured :: Timezone -> Property DebianLike
configured zone = File.hasContent "/etc/timezone" [zone]
	`onChange` update
	`describe` (zone ++ " timezone configured")
  where
	update = Apt.reConfigure "tzdata" mempty
		-- work around a bug in recent tzdata.  See
		-- https://bugs.launchpad.net/ubuntu/+source/tzdata/+bug/1554806/
		`requires` File.notPresent "/etc/localtime"
