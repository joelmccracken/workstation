-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Uwsgi where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

type ConfigFile = [String]

type AppName = String

appEnabled :: AppName -> ConfigFile -> RevertableProperty DebianLike DebianLike
appEnabled an cf = enable <!> disable
  where
	enable = appVal an `File.isSymlinkedTo` appValRelativeCfg an
		`describe` ("uwsgi app enabled " ++ an)
		`requires` appAvailable an cf
		`requires` installed
		`onChange` reloaded
	disable = File.notPresent (appVal an)
		`describe` ("uwsgi app disable" ++ an)
		`requires` installed
		`onChange` reloaded

appAvailable :: AppName -> ConfigFile -> Property DebianLike
appAvailable an cf = ("uwsgi app available " ++ an) ==>
	tightenTargets (appCfg an `File.hasContent` (comment : cf))
  where
	comment = "# deployed with propellor, do not modify"

appCfg :: AppName -> FilePath
appCfg an = "/etc/uwsgi/apps-available" </> an <.> "ini"

appVal :: AppName -> FilePath
appVal an = "/etc/uwsgi/apps-enabled/" </> an <.> "ini"

appValRelativeCfg :: AppName -> File.LinkTarget
appValRelativeCfg an = File.LinkTarget $ "../apps-available" </> an <.> "ini"

installed :: Property DebianLike
installed = Apt.installed ["uwsgi"]

restarted :: Property DebianLike
restarted = Service.restarted "uwsgi"

reloaded :: Property DebianLike
reloaded = Service.reloaded "uwsgi"
