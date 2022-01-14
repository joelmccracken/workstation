{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Property.Apt where

import Data.Maybe
import Data.List
import Data.Typeable
import System.IO
import Control.Monad
import Control.Applicative
import Prelude

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import Propellor.Property.File (Line)
import Propellor.Types.Info
import Utility.SafeCommand

data HostMirror = HostMirror Url
	deriving (Eq, Show, Typeable)

data HostAptProxy = HostAptProxy Url
	deriving (Eq, Show, Typeable)

-- | Indicate host's preferred apt mirror
mirror :: Url -> Property (HasInfo + UnixLike)
mirror u = pureInfoProperty (u ++ " apt mirror selected")
	     (InfoVal (HostMirror u))

getMirror :: Propellor Url
getMirror = do
	mirrorInfo <- getMirrorInfo
	osInfo <- getOS
	return $ case (osInfo, mirrorInfo) of
		(_, Just (HostMirror u)) -> u
		(Just (System (Debian _ _) _), _) ->
			"http://deb.debian.org/debian"
		(Just (System (Buntish _) _), _) ->
			"mirror://mirrors.ubuntu.com/"
		(Just (System dist _), _) ->
			error ("no Apt mirror defined for " ++ show dist)
		_ -> error "no Apt mirror defined for this host or OS"
  where
	getMirrorInfo :: Propellor (Maybe HostMirror)
	getMirrorInfo = fromInfoVal <$> askInfo

withMirror :: Desc -> (Url -> Property DebianLike) -> Property DebianLike
withMirror desc mkp = property' desc $ \w -> do
	u <- getMirror
	ensureProperty w (mkp u)

sourcesList :: FilePath
sourcesList = "/etc/apt/sources.list"

type Url = String
type Section = String

type SourcesGenerator = DebianSuite -> [Line]

showSuite :: DebianSuite -> String
showSuite (Stable s) = s
showSuite Testing = "testing"
showSuite Unstable = "unstable"
showSuite Experimental = "experimental"

backportSuite :: DebianSuite -> Maybe String
backportSuite (Stable s) = Just (s ++ "-backports")
backportSuite _ = Nothing

stableUpdatesSuite :: DebianSuite -> Maybe String
stableUpdatesSuite (Stable s) = Just (s ++ "-updates")
stableUpdatesSuite _ = Nothing

debLine :: String -> Url -> [Section] -> Line
debLine suite url sections = unwords $
	["deb", url, suite] ++ sections

srcLine :: Line -> Line
srcLine l = case words l of
	("deb":rest) -> unwords $ "deb-src" : rest
	_ -> ""

stdSections :: [Section]
stdSections = ["main", "contrib", "non-free"]

binandsrc :: String -> SourcesGenerator
binandsrc url suite = catMaybes
	[ Just l
	, Just $ srcLine l
	, sul
	, srcLine <$> sul
	, bl
	, srcLine <$> bl
	]
  where
	l = debLine (showSuite suite) url stdSections
	bl = do
		bs <- backportSuite suite
		return $ debLine bs url stdSections
	-- formerly known as 'volatile'
	sul = do
		sus <- stableUpdatesSuite suite
		return $ debLine sus url stdSections

stdArchiveLines :: Propellor SourcesGenerator
stdArchiveLines = return . binandsrc =<< getMirror

-- | Only available for Stable suites, not for Testing or Unstable.
securityUpdates :: SourcesGenerator
securityUpdates suite
	| isStable suite =
		let l = "deb http://security.debian.org/debian-security " ++ securitysuite ++ " " ++ unwords stdSections
		in [l, srcLine l]
	| otherwise = []
  where
	securitysuite
		| suite `elem` map Stable releasesusingoldname =
			showSuite suite ++ "/updates"
		| otherwise = showSuite suite ++ "-security"
	releasesusingoldname = ["jessie", "buster", "stretch"]

-- | Makes sources.list have a standard content using the Debian mirror CDN
-- (or other host specified using the `mirror` property), with the
-- Debian suite configured by the os.
stdSourcesList :: Property Debian
stdSourcesList = withOS "standard sources.list" $ \w o -> case o of
	(Just (System (Debian _ suite) _)) ->
		ensureProperty w $ stdSourcesListFor suite
	_ -> unsupportedOS'

stdSourcesListFor :: DebianSuite -> Property Debian
stdSourcesListFor suite = stdSourcesList' suite []

-- | Adds additional sources.list generators.
--
-- Note that if a Property needs to enable an apt source, it's better
-- to do so via a separate file in </etc/apt/sources.list.d/>
stdSourcesList' :: DebianSuite -> [SourcesGenerator] -> Property Debian
stdSourcesList' suite more = tightenTargets $
	withMirror desc $ \u -> setSourcesList
		(concatMap (\gen -> gen suite) (generators u))
  where
	generators u = [binandsrc u, securityUpdates] ++ more
	desc = ("standard sources.list for " ++ show suite)

type PinPriority = Int

-- | Adds an apt source for a suite, and pins that suite to a given pin value
-- (see apt_preferences(5)).  Revert to drop the source and unpin the suite.
--
-- If the requested suite is the host's OS suite, the suite is pinned, but no
-- source is added.  That apt source should already be available, or you can use
-- a property like 'Apt.stdSourcesList'.
suiteAvailablePinned
	:: DebianSuite
	-> PinPriority
	-> RevertableProperty Debian Debian
suiteAvailablePinned s pin = available <!> unavailable
  where
	available :: Property Debian
	available = tightenTargets $ combineProperties (desc True) $ props
		& File.hasContent prefFile (suitePinBlock "*" s pin)
		& setSourcesFile

	unavailable :: Property Debian
	unavailable = tightenTargets $ combineProperties (desc False) $ props
		& File.notPresent sourcesFile
			`onChange` update
		& File.notPresent prefFile

	setSourcesFile :: Property Debian
	setSourcesFile = tightenTargets $ withMirror (desc True) $ \u ->
		withOS (desc True) $ \w o -> case o of
			(Just (System (Debian _ hostSuite) _))
				| s /= hostSuite -> ensureProperty w $
					File.hasContent sourcesFile (sources u)
					`onChange` update
			_ -> noChange

	-- Unless we are pinning a backports suite, filter out any backports
	-- sources that were added by our generators.  The user probably doesn't
	-- want those to be pinned to the same value
	sources u = dropBackports $ concatMap (\gen -> gen s) (generators u)
	  where
		dropBackports
			| "-backports" `isSuffixOf` (showSuite s) = id
			| otherwise = filter (not . isInfixOf "-backports")

	generators u = [binandsrc u, securityUpdates]
	prefFile = "/etc/apt/preferences.d/20" ++ showSuite s ++ ".pref"
	sourcesFile = "/etc/apt/sources.list.d/" ++ showSuite s ++ ".list"

	desc True = "Debian " ++ showSuite s ++ " pinned, priority " ++ show pin
	desc False = "Debian " ++ showSuite s ++ " not pinned"

setSourcesList :: [Line] -> Property DebianLike
setSourcesList ls = sourcesList `File.hasContent` ls `onChange` update

setSourcesListD :: [Line] -> FilePath -> Property DebianLike
setSourcesListD ls basename = f `File.hasContent` ls `onChange` update
  where
	f = "/etc/apt/sources.list.d/" ++ basename ++ ".list"

runApt :: [String] -> UncheckedProperty DebianLike
runApt ps = tightenTargets $ cmdPropertyEnv "apt-get" ps noninteractiveEnv

noninteractiveEnv :: [(String, String)]
noninteractiveEnv =
		[ ("DEBIAN_FRONTEND", "noninteractive")
		, ("APT_LISTCHANGES_FRONTEND", "none")
		]

-- | Have apt update its lists of packages, but without upgrading anything.
update :: Property DebianLike
update = combineProperties desc $ props
	& pendingConfigured
	& aptupdate
  where
	desc = "apt update"
	aptupdate :: Property DebianLike
	aptupdate = withOS desc $ \w o -> case o of
		(Just (System (Debian _ suite) _))
			| not (isStable suite) -> ensureProperty w $
				-- rolling suites' release info can change
				runApt ["update", "--allow-releaseinfo-change"]
					`assume` MadeChange
		_ -> ensureProperty w $ 
			runApt ["update"]
				`assume` MadeChange

-- | Have apt upgrade packages, adding new packages and removing old as
-- necessary. Often used in combination with the `update` property.
upgrade :: Property DebianLike
upgrade = upgrade' "dist-upgrade"

upgrade' :: String -> Property DebianLike
upgrade' p = combineProperties ("apt " ++ p) $ props
	& pendingConfigured
	& runApt ["-y", p]
		`assume` MadeChange

-- | Have apt upgrade packages, but never add new packages or remove
-- old packages. Not suitable for upgrading acrocess major versions
-- of the distribution.
safeUpgrade :: Property DebianLike
safeUpgrade = upgrade' "upgrade"

-- | Have dpkg try to configure any packages that are not fully configured.
pendingConfigured :: Property DebianLike
pendingConfigured = tightenTargets $
	cmdPropertyEnv "dpkg" ["--configure", "--pending"] noninteractiveEnv
		`assume` MadeChange
		`describe` "dpkg configured pending"

type Package = String

installed :: [Package] -> Property DebianLike
installed = installed' ["-y"]

-- | Minimal install of package, without recommends.
installedMin :: [Package] -> Property DebianLike
installedMin = installed' ["--no-install-recommends", "-y"]

installed' :: [String] -> [Package] -> Property DebianLike
installed' params ps = robustly $ check (not <$> isInstalled' ps) go
	`describe` unwords ("apt installed":ps)
  where
	go = runApt (params ++ ["install"] ++ ps)

-- | Install packages from the stable-backports suite.
--
-- If installing the backport requires installing versions of a package's
-- dependencies from stable-backports too, you will need to include those
-- dependencies in the list of packages passed to this function.
backportInstalled :: [Package] -> Property Debian
backportInstalled = backportInstalled' ["-y"]

-- | Minimal install from the stable-backports suite, without recommends.
backportInstalledMin :: [Package] -> Property Debian
backportInstalledMin = backportInstalled' ["--no-install-recommends", "-y"]

backportInstalled' :: [String] -> [Package] -> Property Debian
backportInstalled' params ps = withOS desc $ \w o -> case o of
	(Just (System (Debian _ suite) _)) -> case backportSuite suite of
		Nothing -> unsupportedOS'
		Just bs -> ensureProperty w $
			runApt (("install":params) ++ ((++ '/':bs) <$> ps))
				`changesFile` dpkgStatus
	_ -> unsupportedOS'
  where
	desc = unwords ("apt installed backport":ps)

removed :: [Package] -> Property DebianLike
removed ps = check (any (== IsInstalled) <$> getInstallStatus ps)
	(runApt (["-y", "remove"] ++ ps))
	`describe` unwords ("apt removed":ps)

buildDep :: [Package] -> Property DebianLike
buildDep ps = robustly $ go
	`changesFile` dpkgStatus
	`describe` unwords ("apt build-dep":ps)
  where
	go = runApt $ ["-y", "build-dep"] ++ ps

-- | Installs the build deps for the source package unpacked
-- in the specifed directory, with a dummy package also
-- installed so that autoRemove won't remove them.
buildDepIn :: FilePath -> Property DebianLike
buildDepIn dir = go
	`changesFile` dpkgStatus
	`requires` installedMin ["devscripts", "equivs"]
  where
	-- mk-build-deps may leave files behind sometimes, eg on failure,
	-- so run it in a temp directory, passing the path to the control
	-- file
	go :: UncheckedProperty DebianLike
	go = unchecked $ property ("build-dep in " ++ dir) $ liftIO $
		withTmpDir "build-dep" $ \tmpdir -> do
			cmdResult <$> boolSystem' "mk-build-deps"
				[ File $ dir </> "debian" </> "control"
				, Param "--install"
				, Param "--tool"
				, Param "apt-get -y --no-install-recommends"
				] (\p -> p { cwd = Just tmpdir })

-- | The name of a package, a glob to match the names of packages, or a regexp
-- surrounded by slashes to match the names of packages.  See
-- apt_preferences(5), "Regular expressions and glob(7) syntax"
type AptPackagePref = String

-- | Pins a list of packages, package wildcards and/or regular expressions to a
-- list of suites and corresponding pin priorities (see apt_preferences(5)).
-- Revert to unpin.
--
-- Each package, package wildcard or regular expression will be pinned to all of
-- the specified suites.
--
-- Note that this will have no effect unless there is an apt source for each of
-- the suites.  One way to add an apt source is 'Apt.suiteAvailablePinned'.
--
-- For example, to obtain Emacs Lisp addon packages not present in your release
-- of Debian from testing, falling back to sid if they're not available in
-- testing, you could use
--
--  > & Apt.suiteAvailablePinned Testing (-10)
--  > & Apt.suiteAvailablePinned Unstable (-10)
--  > & ["elpa-*"] `Apt.pinnedTo` [(Testing, 100), (Unstable, 50)]
pinnedTo
	:: [AptPackagePref]
	-> [(DebianSuite, PinPriority)]
	-> RevertableProperty Debian Debian
pinnedTo ps pins = mconcat (map (\p -> pinnedTo' p pins) ps)
	`describe` unwords (("pinned to " ++ showSuites):ps)
  where
	showSuites = intercalate "," $ showSuite . fst <$> pins

pinnedTo'
	:: AptPackagePref
	-> [(DebianSuite, PinPriority)]
	-> RevertableProperty Debian Debian
pinnedTo' p pins =
	(tightenTargets $ prefFile `File.hasContent` prefs)
	<!> (tightenTargets $ File.notPresent prefFile)
  where
	prefs = foldr step [] pins
	step (suite, pin) ls = ls ++ suitePinBlock p suite pin ++ [""]
	prefFile = "/etc/apt/preferences.d/10propellor_"
		++ File.configFileName p <.> "pref"

-- | Package installation may fail becuse the archive has changed.
-- Run an update in that case and retry.
robustly :: Property DebianLike -> Property DebianLike
robustly p = p `fallback` (update `before` p)

isInstalled :: Package -> IO Bool
isInstalled p = isInstalled' [p]

isInstalled' :: [Package] -> IO Bool
isInstalled' ps = do
	is <- getInstallStatus ps
	return $ all (== IsInstalled) is && length is == length ps

data InstallStatus = IsInstalled | NotInstalled
	deriving (Show, Eq)

{- Returns the InstallStatus of packages that are installed
 - or known and not installed. If a package is not known at all to apt
 - or dpkg, it is not included in the list. -}
getInstallStatus :: [Package] -> IO [InstallStatus]
getInstallStatus ps = mapMaybe parse . lines <$> policy
  where
	parse l
		| "Installed: (none)" `isInfixOf` l = Just NotInstalled
		| "Installed: " `isInfixOf` l = Just IsInstalled
		| otherwise = Nothing
	policy = do
		environ <- addEntry "LANG" "C" <$> getEnvironment
		readProcessEnv "apt-cache" ("policy":ps) (Just environ)

autoRemove :: Property DebianLike
autoRemove = runApt ["-y", "autoremove"]
	`changesFile` dpkgStatus
	`describe` "apt autoremove"

-- | Enables unattended upgrades. Revert to disable.
unattendedUpgrades :: RevertableProperty DebianLike DebianLike
unattendedUpgrades = enable <!> disable
  where
	enable = setup True
		`before` Service.running "cron"
		`before` configure
		-- work around http://bugs.debian.org/812380
		`before` File.notPresent "/etc/apt/apt.conf.d/50unattended-upgrades.ucf-dist"
	disable = setup False

	setup enabled = (if enabled then installed else removed) ["unattended-upgrades"]
		`onChange` reConfigure "unattended-upgrades"
			[("unattended-upgrades/enable_auto_updates" , "boolean", v)]
		`describe` ("unattended upgrades " ++ v)
	  where
		v
			| enabled = "true"
			| otherwise = "false"

	configure :: Property DebianLike
	configure = propertyList "unattended upgrades configured" $ props
		& enableupgrading
		& unattendedconfig `File.containsLine` "Unattended-Upgrade::Mail \"root\";"
	  where
		enableupgrading :: Property DebianLike
		enableupgrading = withOS "unattended upgrades configured" $ \w o ->
			case o of
				-- the package defaults to only upgrading stable
				(Just (System (Debian _ suite) _))
					| not (isStable suite) -> ensureProperty w $
						unattendedconfig
							`File.containsLine`
						("Unattended-Upgrade::Origins-Pattern { \"o=Debian,a="++showSuite suite++"\"; };")
				_ -> noChange
		unattendedconfig = "/etc/apt/apt.conf.d/50unattended-upgrades"

-- | Enable periodic updates (but not upgrades), including download
-- of packages.
periodicUpdates :: Property DebianLike
periodicUpdates = tightenTargets $ "/etc/apt/apt.conf.d/02periodic" `File.hasContent`
	[ "APT::Periodic::Enable \"1\";"
	, "APT::Periodic::Update-Package-Lists \"1\";"
	, "APT::Periodic::Download-Upgradeable-Packages \"1\";"
	, "APT::Periodic::Verbose \"1\";"
	]

type DebconfTemplate = String
type DebconfTemplateType = String
type DebconfTemplateValue = String

-- | Preseeds debconf values and reconfigures the package so it takes
-- effect.
reConfigure :: Package -> [(DebconfTemplate, DebconfTemplateType, DebconfTemplateValue)] -> Property DebianLike
reConfigure package vals = tightenTargets $
	reconfigure
		`requires` setselections
		`describe` ("reconfigure " ++ package)
  where
	setselections :: Property DebianLike
	setselections = property "preseed" $
		if null vals
			then noChange
			else makeChange $
				withHandle StdinHandle createProcessSuccess
					(proc "debconf-set-selections" []) $ \h -> do
						forM_ vals $ \(tmpl, tmpltype, value) ->
							hPutStrLn h $ unwords [package, tmpl, tmpltype, value]
						hClose h
	reconfigure = cmdPropertyEnv "dpkg-reconfigure" ["-fnone", package] noninteractiveEnv
		`assume` MadeChange

-- | Ensures that a service is installed and running.
--
-- Assumes that there is a 1:1 mapping between service names and apt
-- package names.
serviceInstalledRunning :: Package -> Property DebianLike
serviceInstalledRunning svc = Service.running svc `requires` installed [svc]

data AptKey = AptKey
	{ keyname :: String
	, pubkey :: String
	}

trustsKey :: AptKey -> RevertableProperty DebianLike DebianLike
trustsKey k = trustsKey' k <!> untrustKey k

trustsKey' :: AptKey -> Property DebianLike
trustsKey' k = check (not <$> doesFileExist f) $ property desc $ makeChange $ do
	withHandle StdinHandle createProcessSuccess
		(proc "apt-key" ["--keyring", f, "add", "-"]) $ \h -> do
			hPutStr h (pubkey k)
			hClose h
	nukeFile $ f ++ "~" -- gpg dropping
  where
	desc = "apt trusts key " ++ keyname k
	f = aptKeyFile k

untrustKey :: AptKey -> Property DebianLike
untrustKey = tightenTargets . File.notPresent . aptKeyFile

aptKeyFile :: AptKey -> FilePath
aptKeyFile k = "/etc/apt/trusted.gpg.d" </> keyname k ++ ".gpg"

-- | Cleans apt's cache of downloaded packages to avoid using up disk
-- space.
cacheCleaned :: Property DebianLike
cacheCleaned = tightenTargets $ cmdProperty "apt-get" ["clean"]
	`assume` NoChange
	`describe` "apt cache cleaned"

-- | Add a foreign architecture to dpkg and apt.
hasForeignArch :: String -> Property DebianLike
hasForeignArch arch = check notAdded (add `before` update)
	`describe` ("dpkg has foreign architecture " ++ arch)
  where
	notAdded = (notElem arch . lines) <$> readProcess "dpkg" ["--print-foreign-architectures"]
	add = cmdProperty "dpkg" ["--add-architecture", arch]
		`assume` MadeChange

-- | Disable the use of PDiffs for machines with high-bandwidth connections.
noPDiffs :: Property DebianLike
noPDiffs = tightenTargets $ "/etc/apt/apt.conf.d/20pdiffs" `File.hasContent`
	[ "Acquire::PDiffs \"false\";" ]

suitePin :: DebianSuite -> String
suitePin s = prefix s ++ showSuite s
  where
	prefix (Stable _) = "n="
	prefix _ = "a="

suitePinBlock :: AptPackagePref -> DebianSuite -> PinPriority -> [Line]
suitePinBlock p suite pin =
	[ "Explanation: This file added by propellor"
	, "Package: " ++ p
	, "Pin: release " ++ suitePin suite
	, "Pin-Priority: " ++ val pin
	]

dpkgStatus :: FilePath
dpkgStatus = "/var/lib/dpkg/status"

-- | Set apt's proxy
proxy :: Url -> Property (HasInfo + DebianLike)
proxy u = setInfoProperty (proxy' u) (proxyInfo u)
  where
	proxyInfo = toInfo . InfoVal . HostAptProxy

proxy' :: Url -> Property DebianLike
proxy' u = tightenTargets $
	"/etc/apt/apt.conf.d/20proxy" `File.hasContent`
		[ "Acquire::HTTP::Proxy \"" ++ u ++ "\";" ]
		`describe` desc
  where
	desc = (u ++ " apt proxy selected")

-- | Cause apt to proxy downloads via an apt cacher on localhost
useLocalCacher :: Property (HasInfo + DebianLike)
useLocalCacher = proxy "http://localhost:3142"
	`requires` serviceInstalledRunning "apt-cacher-ng"
	`describe` "apt uses local apt cacher"
