-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.DebianMirror
	( DebianPriority (..)
	, showPriority
	, mirror
	, RsyncExtra (..)
	, Method (..)
	, DebianMirror
	, debianMirrorHostName
	, debianMirrorSuites
	, debianMirrorArchitectures
	, debianMirrorSections
	, debianMirrorSourceBool
	, debianMirrorPriorities
	, debianMirrorMethod
	, debianMirrorKeyring
	, debianMirrorRsyncExtra
	, mkDebianMirror
	) where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.User as User

import Data.List


data DebianPriority = Essential | Required | Important | Standard | Optional | Extra
	deriving (Show, Eq)

showPriority :: DebianPriority -> String
showPriority Essential = "essential"
showPriority Required  = "required"
showPriority Important = "important"
showPriority Standard  = "standard"
showPriority Optional  = "optional"
showPriority Extra     = "extra"

data RsyncExtra = Doc | Indices | Tools | Trace
	deriving (Show, Eq)

showRsyncExtra :: RsyncExtra -> String
showRsyncExtra Doc = "doc"
showRsyncExtra Indices = "indices"
showRsyncExtra Tools = "tools"
showRsyncExtra Trace = "trace"

data Method = Ftp | Http | Https | Rsync | MirrorFile

showMethod :: Method -> String
showMethod Ftp = "ftp"
showMethod Http = "http"
showMethod Https = "https"
showMethod Rsync = "rsync"
showMethod MirrorFile = "file"

-- | To get a new DebianMirror and set options, use:
--
-- > mkDebianMirror mymirrordir mycrontimes
-- > 	. debianMirrorHostName "otherhostname"
-- > 	. debianMirrorSourceBool True

data DebianMirror = DebianMirror
	{ _debianMirrorHostName :: HostName
	, _debianMirrorDir :: FilePath
	, _debianMirrorSuites :: [DebianSuite]
	, _debianMirrorArchitectures :: [Architecture]
	, _debianMirrorSections :: [Apt.Section]
	, _debianMirrorSourceBool :: Bool
	, _debianMirrorPriorities :: [DebianPriority]
	, _debianMirrorMethod :: Method
	, _debianMirrorKeyring :: FilePath
	, _debianMirrorRsyncExtra :: [RsyncExtra]
	, _debianMirrorCronTimes :: Cron.Times
	}

mkDebianMirror :: FilePath -> Cron.Times -> DebianMirror
mkDebianMirror dir crontimes = DebianMirror
	{ _debianMirrorHostName = "deb.debian.org"
	, _debianMirrorDir = dir
	, _debianMirrorSuites = []
	, _debianMirrorArchitectures = []
	, _debianMirrorSections = []
	, _debianMirrorSourceBool = False
	, _debianMirrorPriorities = []
	, _debianMirrorMethod = Http
	, _debianMirrorKeyring = "/usr/share/keyrings/debian-archive-keyring.gpg"
	, _debianMirrorRsyncExtra = [Trace]
	, _debianMirrorCronTimes = crontimes
	}

debianMirrorHostName :: HostName -> DebianMirror -> DebianMirror
debianMirrorHostName hn m = m { _debianMirrorHostName = hn }

debianMirrorSuites :: [DebianSuite] -> DebianMirror -> DebianMirror
debianMirrorSuites s m = m { _debianMirrorSuites = s }

debianMirrorArchitectures :: [Architecture] -> DebianMirror -> DebianMirror
debianMirrorArchitectures a m = m { _debianMirrorArchitectures = a }

debianMirrorSections :: [Apt.Section] -> DebianMirror -> DebianMirror
debianMirrorSections s m = m { _debianMirrorSections = s }

debianMirrorSourceBool :: Bool -> DebianMirror -> DebianMirror
debianMirrorSourceBool s m = m { _debianMirrorSourceBool = s }

debianMirrorPriorities :: [DebianPriority] -> DebianMirror -> DebianMirror
debianMirrorPriorities p m = m { _debianMirrorPriorities = p }

debianMirrorMethod :: Method -> DebianMirror -> DebianMirror
debianMirrorMethod me m = m { _debianMirrorMethod = me }

debianMirrorKeyring :: FilePath -> DebianMirror -> DebianMirror
debianMirrorKeyring k m = m { _debianMirrorKeyring = k }

debianMirrorRsyncExtra :: [RsyncExtra] -> DebianMirror -> DebianMirror
debianMirrorRsyncExtra r m = m { _debianMirrorRsyncExtra = r }

mirror :: DebianMirror -> Property DebianLike
mirror mirror' = propertyList ("Debian mirror " ++ dir) $ props
	& Apt.installed ["debmirror"]
	& User.accountFor (User "debmirror")
	& File.dirExists dir
	& File.ownerGroup dir (User "debmirror") (Group "debmirror")
	& check (not . and <$> mapM suitemirrored suites)
		(cmdProperty "debmirror" args)
			`describe` "debmirror setup"
	& Cron.niceJob ("debmirror_" ++ dir) (_debianMirrorCronTimes mirror') (User "debmirror") "/"
		(unwords ("/usr/bin/debmirror" : args))
  where
	dir = _debianMirrorDir mirror'
	suites = _debianMirrorSuites mirror'
	suitemirrored suite = doesDirectoryExist $ dir </> "dists" </> Apt.showSuite suite
	architecturearg = intercalate ","
	suitearg = intercalate "," $ map Apt.showSuite suites
	priorityRegex pp = "(" ++ intercalate "|" (map showPriority pp) ++ ")"
	rsyncextraarg [] = "none"
	rsyncextraarg res = intercalate "," $ map showRsyncExtra res
	args =
		[ "--dist" , suitearg
		, "--arch", architecturearg $ map architectureToDebianArchString (_debianMirrorArchitectures mirror')
		, "--section", intercalate "," $ _debianMirrorSections mirror'
		, "--limit-priority", "\"" ++ priorityRegex (_debianMirrorPriorities mirror') ++ "\""
		]
		++
		(if _debianMirrorSourceBool mirror' then [] else ["--nosource"])
		++
		[ "--host", _debianMirrorHostName mirror'
		, "--method", showMethod $ _debianMirrorMethod mirror'
		, "--rsync-extra", rsyncextraarg $ _debianMirrorRsyncExtra mirror'
		, "--keyring", _debianMirrorKeyring mirror'
		, dir
		]
