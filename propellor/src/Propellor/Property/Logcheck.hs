-- | Maintainer: Jelmer Vernooĳ <jelmer@jelmer.uk>

module Propellor.Property.Logcheck (
	ReportLevel (Workstation, Server, Paranoid),
	Service,
	defaultPrefix,
	ignoreFilePath,
	ignoreLines,
	installed,
) where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File

data ReportLevel = Workstation | Server | Paranoid
type Service = String

instance ConfigurableValue ReportLevel where
	val Workstation = "workstation"
	val Server = "server"
	val Paranoid = "paranoid"

-- The common prefix used by default in syslog lines.
defaultPrefix :: String
defaultPrefix = "^\\w{3} [ :[:digit:]]{11} [._[:alnum:]-]+ "

ignoreFilePath :: ReportLevel -> Service -> FilePath
ignoreFilePath t n = "/etc/logcheck/ignore.d." ++ (val t) </> n

ignoreLines :: ReportLevel -> Service -> [String] -> Property UnixLike
ignoreLines t n ls = (ignoreFilePath t n) `File.containsLines` ls
	`describe` ("logcheck ignore lines for " ++ n ++ "(" ++ val t ++ ")")

installed :: Property DebianLike
installed = Apt.installed ["logcheck"]
