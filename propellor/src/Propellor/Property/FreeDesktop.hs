-- | Freedesktop.org configuration file properties.

module Propellor.Property.FreeDesktop where

import Propellor.Base
import Propellor.Property.ConfFile

desktopFile :: String -> FilePath
desktopFile s = s ++ ".desktop"

-- | Name used in a desktop file; user visible. 
type Name = String

-- | Command that a dekstop file runs. May include parameters.
type Exec = String

-- | Specifies an autostart file. By default it will be located in the
-- system-wide autostart directory.
autostart :: FilePath -> Name -> Exec -> RevertableProperty UnixLike UnixLike
autostart f n e = ("/etc/xdg/autostart" </> f) `iniFileContains` 
	[ ("Desktop Entry",
		[ ("Type", "Application")
		, ("Version", "1.0")
		, ("Name", n)
		, ("Comment", "Autostart")
		, ("Terminal", "False")
		, ("Exec", e)
		] )
	]
