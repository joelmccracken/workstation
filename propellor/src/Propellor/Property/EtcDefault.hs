-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.EtcDefault (
	EtcDefaultFile,
	set,
) where

import Propellor.Base
import Propellor.Property.ConfFile

-- | The name of a file in </etc/default>, without the </etc/default> prefix.
-- E.g. @useradd@.
type EtcDefaultFile = String

-- | Set a key=value pair in a file in </etc/default>.
set :: EtcDefaultFile -> ShellKey -> String -> Property UnixLike
set f key value = containsShellSetting ("/etc/default" </> f) (key, value)
