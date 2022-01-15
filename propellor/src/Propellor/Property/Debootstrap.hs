{-# LANGUAGE TypeFamilies #-}

module Propellor.Property.Debootstrap (
	Url,
	DebootstrapConfig(..),
	built,
	built',
	extractSuite,
	installed,
	sourceInstall,
) where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.Qemu
import Utility.Path

import Data.List
import Data.Char
import qualified Data.Semigroup as Sem
import System.Posix.Directory
import System.Posix.Files
import Propellor.Property.Mount

type Url = String

-- | A data type for debootstrap configuration.
-- mempty is a default debootstrapped system.
data DebootstrapConfig
	= DefaultConfig
	| MinBase
	| BuilddD
	| DebootstrapParam String
	| UseEmulation
	| DebootstrapProxy Url
	| DebootstrapMirror Url
	| DebootstrapConfig :+ DebootstrapConfig
	deriving (Show)

instance Sem.Semigroup DebootstrapConfig where
	(<>) = (:+)

instance Monoid DebootstrapConfig where
	mempty  = DefaultConfig
	mappend = (Sem.<>)

toParams :: DebootstrapConfig -> [CommandParam]
toParams DefaultConfig = []
toParams MinBase = [Param "--variant=minbase"]
toParams BuilddD = [Param "--variant=buildd"]
toParams (DebootstrapParam p) = [Param p]
toParams UseEmulation = []
toParams (DebootstrapProxy _) = []
toParams (DebootstrapMirror _) = []
toParams (c1 :+ c2) = toParams c1 <> toParams c2

useEmulation :: DebootstrapConfig -> Bool
useEmulation UseEmulation = True
useEmulation (a :+ b) = useEmulation a || useEmulation b
useEmulation _ = False

debootstrapProxy :: DebootstrapConfig -> Maybe Url
debootstrapProxy (DebootstrapProxy u) = Just u
debootstrapProxy (a :+ b) = debootstrapProxy a <|> debootstrapProxy b
debootstrapProxy _ = Nothing

debootstrapMirror :: DebootstrapConfig -> Maybe Url
debootstrapMirror (DebootstrapMirror u) = Just u
debootstrapMirror (a :+ b) = debootstrapMirror a <|> debootstrapMirror b
debootstrapMirror _ = Nothing

-- | Builds a chroot in the given directory using debootstrap.
--
-- The System can be any OS and architecture that debootstrap
-- and the kernel support.
--
-- When the System is architecture that the kernel does not support,
-- it can still be bootstrapped using emulation. This is determined
-- by checking `supportsArch`, or can be configured with `UseEmulation`.
built :: FilePath -> System -> DebootstrapConfig -> Property Linux
built target system@(System _ targetarch) config =
	withOS ("debootstrapped " ++ target) go
  where
	go w (Just hostos)
		| supportsArch hostos targetarch && not (useEmulation config) =
			ensureProperty w $
				built' (setupRevertableProperty installed)
					target system config
	go w _ = ensureProperty w $ do
		let p = setupRevertableProperty foreignBinariesEmulated
			`before` setupRevertableProperty installed
		built' p target system (config :+ UseEmulation)

-- | Like `built`,  but uses the provided Property to install debootstrap.
built' :: Property Linux -> FilePath -> System -> DebootstrapConfig -> Property Linux
built' installprop target system@(System _ arch) config = 
	go `before` oldpermfix
  where
	go = check (isUnpopulated target <||> ispartial) setupprop
		`requires` installprop

	setupprop :: Property Linux
	setupprop = property ("debootstrapped " ++ target) $ liftIO $ do
		createDirectoryIfMissing True target
		suite <- case extractSuite system of
			Nothing -> errorMessage $ "don't know how to debootstrap " ++ show system
			Just s -> pure s
		let params = toParams config ++
			[ Param $ "--arch=" ++ architectureToDebianArchString arch
			, Param suite
			, Param target
			] ++ case debootstrapMirror config of
				Just u -> [Param u]
				Nothing -> []
		cmd <- fromMaybe "debootstrap" <$> programPath
		de <- case debootstrapProxy config of
			Just u -> addEntry "http_proxy" u <$> standardPathEnv
			Nothing -> standardPathEnv
		ifM (boolSystemEnv cmd params (Just de))
			( return MadeChange
			, return FailedChange
			)

	-- A failed debootstrap run will leave a debootstrap directory;
	-- recover by deleting it and trying again.
	ispartial = ifM (doesDirectoryExist (target </> "debootstrap"))
		( do
			removeChroot target
			return True
		, return False
		)
	
	-- May want to remove this after some appropriate length of time,
	-- as it's a workaround for chroots set up with too tight
	-- permissions.
	oldpermfix :: Property Linux
	oldpermfix = property ("fixed old chroot file mode") $ do
		liftIO $ modifyFileMode target $
			addModes [otherReadMode, otherExecuteMode]
		return NoChange

extractSuite :: System -> Maybe String
extractSuite (System (Debian _ s) _) = Just $ Apt.showSuite s
extractSuite (System (Buntish r) _) = Just r
extractSuite (System (ArchLinux) _) = Nothing
extractSuite (System (FreeBSD _) _) = Nothing

-- | Ensures debootstrap is installed.
--
-- When necessary, falls back to installing debootstrap from source.
-- Note that installation from source is done by downloading the tarball
-- from a Debian mirror, with no cryptographic verification.
installed :: RevertableProperty Linux Linux
installed = install <!> remove
  where
	install = check (isNothing <$> programPath) $
		(aptinstall `pickOS` sourceInstall)
			`describe` "debootstrap installed"

	remove = (aptremove `pickOS` sourceRemove)
		`describe` "debootstrap removed"

	aptinstall = Apt.installed ["debootstrap"]
	aptremove = Apt.removed ["debootstrap"]

sourceInstall :: Property Linux
sourceInstall = go
	`requires` perlInstalled
	`requires` arInstalled
  where
	go :: Property Linux
	go = property "debootstrap installed from source" (liftIO sourceInstall')

perlInstalled :: Property Linux
perlInstalled = check (not <$> inPath "perl") $ property "perl installed" $
	liftIO $ toResult . isJust <$> firstM id
		[ yumInstall "perl"
		]

arInstalled :: Property Linux
arInstalled = check (not <$> inPath "ar") $ property "ar installed" $
	liftIO $ toResult . isJust <$> firstM id
		[ yumInstall "binutils"
		]

yumInstall :: String -> IO Bool
yumInstall p = boolSystem "yum" [Param "-y", Param "install", Param p]

sourceInstall' :: IO Result
sourceInstall' = withTmpDir "debootstrap" $ \tmpd -> do
	let indexfile = tmpd </> "index.html"
	unlessM (download baseurl indexfile) $
		errorMessage $ "Failed to download " ++ baseurl
	urls <- sortBy (flip compare) -- highest version first
		. filter ("debootstrap_" `isInfixOf`)
		. filter (".tar." `isInfixOf`)
		. extractUrls baseurl <$>
		readFileStrict indexfile
	nukeFile indexfile

	tarfile <- case urls of
		(tarurl:_) -> do
			let f = tmpd </> takeFileName tarurl
			unlessM (download tarurl f) $
				errorMessage $ "Failed to download " ++ tarurl
			return f
		_ -> errorMessage $ "Failed to find any debootstrap tarballs listed on " ++ baseurl

	createDirectoryIfMissing True localInstallDir
	bracket getWorkingDirectory changeWorkingDirectory $ \_ -> do
		changeWorkingDirectory localInstallDir
		unlessM (boolSystem "tar" [Param "xf", File tarfile]) $
			errorMessage "Failed to extract debootstrap tar file"
		nukeFile tarfile
		l <- dirContents "."
		case l of
			(subdir:[]) -> do
				changeWorkingDirectory subdir
				makeWrapperScript (localInstallDir </> subdir)
				return MadeChange
			_ -> errorMessage "debootstrap tar file did not contain exactly one directory"

sourceRemove :: Property Linux
sourceRemove = property "debootstrap not installed from source" $ liftIO $
	ifM (doesDirectoryExist sourceInstallDir)
		( do
			removeDirectoryRecursive sourceInstallDir
			return MadeChange
		, return NoChange
		)

sourceInstallDir :: FilePath
sourceInstallDir = "/usr/local/propellor/debootstrap"

wrapperScript :: FilePath
wrapperScript = sourceInstallDir </> "debootstrap.wrapper"

-- | Finds debootstrap in PATH, but fall back to looking for the
-- wrapper script that is installed, outside the PATH, when debootstrap
-- is installed from source.
programPath :: IO (Maybe FilePath)
programPath = getM searchPath
	[ "debootstrap"
	, wrapperScript
	]

makeWrapperScript :: FilePath -> IO ()
makeWrapperScript dir = do
	createDirectoryIfMissing True (takeDirectory wrapperScript)
	writeFile wrapperScript $ unlines
		[ "#!/bin/sh"
		, "set -e"
		, "DEBOOTSTRAP_DIR=" ++ dir
		, "export DEBOOTSTRAP_DIR"
		, dir </> "debootstrap" ++ " \"$@\""
		]
	modifyFileMode wrapperScript (addModes $ readModes ++ executeModes)

localInstallDir :: FilePath
localInstallDir = "/usr/local/debootstrap"

-- This http server directory listing is relied on to be fairly sane,
-- which is one reason why it's using a specific server and not a
-- round-robin address.
baseurl :: Url
baseurl = "http://ftp.debian.org/debian/pool/main/d/debootstrap/"

download :: Url -> FilePath -> IO Bool
download url dest = anyM id
	[ boolSystem "curl" [Param "-o", File dest, Param url]
	, boolSystem "wget" [Param "-O", File dest, Param url]
	]

-- Pretty hackish, but I don't want to pull in a whole html parser
-- or parsec dependency just for this.
--
-- To simplify parsing, lower case everything. This is ok because
-- the filenames are all lower-case anyway.
extractUrls :: Url -> String -> [Url]
extractUrls base = collect [] . map toLower
  where
	collect l [] = l
	collect l ('h':'r':'e':'f':'=':r) = case r of
		('"':r') -> findend l r'
		_ -> findend l r
	collect l (_:cs) = collect l cs

	findend l s =
		let (u, r) = break (== '"') s
		    u' = if "http" `isPrefixOf` u
			then u
			else base </> u
		in collect (u':l) r


stdPATH :: String
stdPATH = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

-- | Removes the contents of a chroot. First, unmounts any filesystems
-- mounted within it.
removeChroot :: FilePath -> IO ()
removeChroot c = do
	unmountBelow c
	removeDirectoryRecursive c

-- | When chrooting, it's useful to ensure that PATH has all the standard
-- directories in it. This adds those directories to whatever PATH is
-- already set.
standardPathEnv :: IO [(String, String)]
standardPathEnv = do
	path <- getEnvDefault "PATH" "/bin"
	addEntry "PATH" (path ++ stdPATH)
		<$> getEnvironment
