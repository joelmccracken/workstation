module Propellor.Property.Fail2Ban where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import Propellor.Property.ConfFile

installed :: Property DebianLike
installed = Apt.serviceInstalledRunning "fail2ban"

reloaded :: Property DebianLike
reloaded = Service.reloaded "fail2ban"

type Jail = String

type Filter = String

type Action = String

-- | By default, fail2ban only enables the ssh jail, but many others
-- are available to be enabled, for example "postfix-sasl"
jailEnabled :: Jail -> Property DebianLike
jailEnabled name = jailEnabled' name []
	`onChange` reloaded

jailEnabled' :: Jail -> [(IniKey, String)] -> Property DebianLike
jailEnabled' name settings =
	jailConfigured' name (("enabled", "true") : settings)
	`onChange` reloaded

-- | Configures a jail. For example:
--
-- > jailConfigured' "sshd" [("port", "2222")]
jailConfigured' :: Jail -> [(IniKey, String)] -> Property UnixLike
jailConfigured' name settings = propertyList ("jail \"" ++ name ++ "\" configuration") $ props
	-- removes .conf files added by old versions of Fail2Ban properties
	& File.notPresent (oldJailConfFile name)
	& jailConfFile name `iniFileContains` [(name, settings)]

-- | Adds a setting to a given jail. For example:
--
-- > jailConfigured "sshd" "port"  "2222"
jailConfigured :: Jail -> IniKey -> String -> Property UnixLike
jailConfigured name key value = propertyList ("jail \"" ++ name ++ "\" configuration") $ props
	-- removes .conf files added by old versions of Fail2Ban properties
	& File.notPresent (oldJailConfFile name)
	& jailConfFile name `containsIniSetting` (name, key, value)

oldJailConfFile :: Jail -> FilePath
oldJailConfFile name = "/etc/fail2ban/jail.d/" ++ name ++ ".conf"

jailConfFile :: Jail -> FilePath
jailConfFile name = "/etc/fail2ban/jail.d/" ++ name ++ ".local"

filterConfFile :: Filter -> FilePath
filterConfFile name = "/etc/fail2ban/filter.d/" ++ name ++ ".local"

actionConfFile :: Action -> FilePath
actionConfFile name = "/etc/fail2ban/action.d/" ++ name ++ ".local"
