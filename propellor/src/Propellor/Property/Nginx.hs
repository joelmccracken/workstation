-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Nginx where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

type ConfigFile = [String]

siteEnabled :: HostName -> ConfigFile -> RevertableProperty DebianLike DebianLike
siteEnabled hn cf = enable <!> disable
  where
	enable = siteVal hn `File.isSymlinkedTo` siteValRelativeCfg hn
		`describe` ("nginx site enabled " ++ hn)
		`requires` siteAvailable hn cf
		`requires` installed
		`onChange` reloaded
	disable = File.notPresent (siteVal hn)
		`describe` ("nginx site disable" ++ hn)
		`requires` installed
		`onChange` reloaded

siteAvailable :: HostName -> ConfigFile -> Property DebianLike
siteAvailable hn cf = "nginx site available " ++ hn ==> tightenTargets go
  where
	comment = "# deployed with propellor, do not modify"
	go = siteCfg hn `File.hasContent` (comment : cf)

siteCfg :: HostName -> FilePath
siteCfg hn = "/etc/nginx/sites-available/" ++ hn

siteVal :: HostName -> FilePath
siteVal hn = "/etc/nginx/sites-enabled/" ++ hn

siteValRelativeCfg :: HostName -> File.LinkTarget
siteValRelativeCfg hn = File.LinkTarget ("../sites-available/" ++ hn)

installed :: Property DebianLike
installed = Apt.installed ["nginx"]

restarted :: Property DebianLike
restarted = Service.restarted "nginx"

reloaded :: Property DebianLike
reloaded = Service.reloaded "nginx"
