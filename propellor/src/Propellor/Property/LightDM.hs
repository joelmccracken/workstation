-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.LightDM where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.ConfFile as ConfFile

installed :: Property DebianLike
installed = Apt.installed ["lightdm"]

-- | Configures LightDM to skip the login screen and autologin as a user.
autoLogin :: User -> RevertableProperty DebianLike DebianLike
autoLogin (User u) = (setup <!> cleanup)
	`describe` ("lightdm autologin for " ++ u)
  where
	cf = "/etc/lightdm/lightdm.conf"
	setting = ("Seat:*", "autologin-user", u)
	setup = cf `ConfFile.containsIniSetting` setting
		`requires` installed
	cleanup = tightenTargets $ cf `ConfFile.lacksIniSetting` setting
