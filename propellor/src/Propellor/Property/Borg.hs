-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>
--
-- Support for the Borg backup tool <https://github.com/borgbackup>

module Propellor.Property.Borg
	( BorgParam
	, BorgRepo(..)
	, BorgRepoOpt(..)
	, BorgEnc(..)
	, installed
	, repoExists
	, init
	, restored
	, backup
	, KeepPolicy (..)
	) where

import Propellor.Base hiding (init, last)
import Prelude hiding (init)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Data.List (intercalate, isSuffixOf)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Utility.SafeCommand (boolSystem', toCommand)

-- | Borg command.
type BorgCommand = String

-- | Parameter to pass to a borg command.
type BorgParam = String

-- | A borg repository.
data BorgRepo
	-- | Location of the repository, eg
	-- `BorgRepo "root@myserver:/mnt/backup/git.borg"`
	= BorgRepo String
	-- | Location of the repository, and additional options to use
	-- when accessing the repository.
	| BorgRepoUsing [BorgRepoOpt] String

data BorgRepoOpt 
	-- | Use to specify a ssh private key to use when accessing a
	-- BorgRepo.
	= UseSshKey FilePath
	-- | Use to specify a umask to use when accessing BorgRepo.
	| UseUmask FileMode
	-- | Use to specify an environment variable to set when running
	-- borg on a BorgRepo.
	| UsesEnvVar (String, String)

-- | Borg Encryption type.
data BorgEnc
	-- | No encryption, no authentication.
	= BorgEncNone
	-- | Authenticated, using SHA-256 for hash/MAC.
	| BorgEncAuthenticated
	-- | Authenticated, using Blake2b for hash/MAC.
	| BorgEncAuthenticatedBlake2
	-- | Encrypted, storing the key in the repository, using SHA-256 for
	-- hash/MAC.
	| BorgEncRepokey
	-- | Encrypted, storing the key in the repository, using Blake2b for
	-- hash/MAC.
	| BorgEncRepokeyBlake2
	-- | Encrypted, storing the key outside of the repository, using
	-- SHA-256 for hash/MAC.
	| BorgEncKeyfile
	-- | Encrypted, storing the key outside of the repository, using
	-- Blake2b for hash/MAC.
	| BorgEncKeyfileBlake2

repoLoc :: BorgRepo -> String
repoLoc (BorgRepo s) = s
repoLoc (BorgRepoUsing _ s) = s

runBorg :: BorgRepo -> BorgCommand -> [CommandParam] -> Maybe FilePath -> IO Bool
runBorg repo cmd ps chdir = case runBorgEnv repo of
	[] -> runBorg' Nothing
	environ -> do
		environ' <- addEntries environ <$> getEnvironment
		runBorg' (Just environ')
  where
	runBorg' environ = boolSystem' "borg" params $
		\p -> p { cwd = chdir, env = environ }
	params = runBorgParam repo cmd ps

readBorg :: BorgRepo -> BorgCommand -> [CommandParam] -> IO String
readBorg repo cmd ps = case runBorgEnv repo of
	[] -> readProcess "borg" params
	environ -> do
		environ' <- addEntries environ <$> getEnvironment
		readProcessEnv "borg" params (Just environ')
  where
	params = toCommand (runBorgParam repo cmd ps)

runBorgParam :: BorgRepo -> BorgCommand -> [CommandParam] -> [CommandParam]
runBorgParam (BorgRepo _) cmd ps = Param cmd : ps
runBorgParam (BorgRepoUsing os _) cmd ps = Param cmd : (concatMap go os ++ ps)
  where
	go (UseUmask i) = [Param "--umask", Param (showIntAtBase 8 intToDigit i "")]
	go _ = []

runBorgEnv :: BorgRepo -> [(String, String)]
runBorgEnv (BorgRepo _) = []
runBorgEnv (BorgRepoUsing os _) = mapMaybe go os
  where
	go (UseSshKey k) = Just ("BORG_RSH", "ssh -i " ++ k)
	go (UsesEnvVar (k, v)) = Just (k, v)
	go _ = Nothing

installed :: Property DebianLike
installed = pickOS installdebian aptinstall
  where
	installdebian :: Property Debian
	installdebian = withOS desc $ \w o -> case o of
		(Just (System (Debian _ (Stable "jessie")) _)) -> ensureProperty w $
			Apt.backportInstalled ["borgbackup", "python3-msgpack"]
		_ -> ensureProperty w $
			Apt.installed ["borgbackup"]
	aptinstall = Apt.installed ["borgbackup"] `describe` desc
        desc = "installed borgbackup"

repoExists :: BorgRepo -> IO Bool
repoExists repo = runBorg repo "list" [Param (repoLoc repo)] Nothing

-- | Get the name of the latest archive.
latestArchive :: BorgRepo -> IO (Maybe String)
latestArchive repo = getLatest <$> readBorg repo "list" listargs
  where
	getLatest = maybeLast . filter (not . isSuffixOf ".checkpoint") . lines
	maybeLast [] = Nothing
	maybeLast ps = Just $ last ps
	listargs =
		[ Param "--short"
		, Param (repoLoc repo)
		]

-- | Inits a new borg repository
init :: BorgRepo -> BorgEnc -> Property DebianLike
init repo enc = check (not <$> repoExists repo)
	(cmdPropertyEnv "borg" initargs (runBorgEnv repo))
		`requires` installed
  where
	initargs = toCommand $ runBorgParam repo "init"
		[ encParam enc
		, Param (repoLoc repo)
		]

-- | Restores a directory from a borg backup.
--
-- Only does anything if the directory does not exist, or exists,
-- but is completely empty.
--
-- The restore is performed atomically; restoring to a temp directory
-- and then moving it to the directory.
restored :: FilePath -> BorgRepo -> Property DebianLike
restored dir repo = go `requires` installed
  where
	go :: Property DebianLike
	go = property (dir ++ " restored by borg") $ ifM (liftIO needsRestore)
		( do
			warningMessage $ dir ++ " is empty/missing; restoring from backup ..."
			latest <- liftIO (latestArchive repo)
			case latest of
				Nothing -> do
					warningMessage $ "no archive to extract"
					return FailedChange
				Just l -> liftIO (restore l)
		, noChange
		)

	needsRestore = isUnpopulated dir

	restore :: String -> IO Result
	restore latest = withTmpDirIn (takeDirectory dir) "borg-restore" $ \tmpdir -> do
		ok <- runBorg repo "extract"
			[ Param (repoLoc repo ++ "::" ++ latest) ]
			(Just tmpdir)
		let restoreddir = tmpdir ++ "/" ++ dir
		ifM (pure ok <&&> doesDirectoryExist restoreddir)
			( do
				void $ tryIO $ removeDirectory dir
				renameDirectory restoreddir dir
				return MadeChange
			, return FailedChange
			)

-- | Installs a cron job that causes a given directory to be backed
-- up, by running borg with some parameters.
--
-- If the directory does not exist, or exists but is completely empty,
-- this Property will immediately restore it from an existing backup.
--
-- So, this property can be used to deploy a directory of content
-- to a host, while also ensuring any changes made to it get backed up.
-- For example:
--
-- >	& Borg.backup "/srv/git"
-- >		(BorgRepo "root@myserver:/mnt/backup/git.borg") 
-- >		Cron.Daily
-- >		["--exclude=/srv/git/tobeignored"]
-- >		[Borg.KeepDays 7, Borg.KeepWeeks 4, Borg.KeepMonths 6, Borg.KeepYears 1]
--
-- Note that this property does not initialize the backup repository,
-- so that will need to be done once, before-hand.
--
-- Since borg uses a fair amount of system resources, only one borg
-- backup job will be run at a time. Other jobs will wait their turns to
-- run.
backup :: FilePath -> BorgRepo -> Cron.Times -> [BorgParam] -> [KeepPolicy] -> Property DebianLike
backup dir repo crontimes extraargs kp = backup' dir repo crontimes extraargs kp
	`requires` restored dir repo

-- | Does a backup, but does not automatically restore.
backup' :: FilePath -> BorgRepo -> Cron.Times -> [BorgParam] -> [KeepPolicy] -> Property DebianLike
backup' dir repo crontimes extraargs kp = cronjob
	`describe` desc
	`requires` installed
  where
	desc = repoLoc repo ++ " borg backup"
	cronjob = Cron.niceJob ("borg_backup" ++ dir) crontimes (User "root") "/" $
		"flock " ++ shellEscape lockfile ++ " sh -c " ++ shellEscape backupcmd
	lockfile = "/var/lock/propellor-borg.lock"
	backupcmd = intercalate "&&" $ concat
		[ concatMap exportenv (runBorgEnv repo)
		, [createCommand]
		, if null kp then [] else [pruneCommand]
		]
	exportenv (k, v) = 
		[ k ++ "=" ++ shellEscape v
		, "export " ++ k
		]
	createCommand = unwords ("borg" : createCommandParams)
	createCommandParams = map shellEscape $ toCommand $ runBorgParam repo "create" $
		[ Param "--stats" ]
		++ map Param extraargs ++
		[ Param (repoLoc repo ++ "::{now}")
		, File dir
		]
	pruneCommand = unwords $ ("borg" : pruneCommandParams)
	pruneCommandParams = map shellEscape $ toCommand $ runBorgParam repo "prune" $
		[ Param (repoLoc repo) ]
		++ map keepParam kp

-- | Constructs an BorgParam that specifies which old backup generations to
-- keep. By default, all generations are kept. However, when this parameter is
-- passed to the `backup` property, it will run borg prune to clean out
-- generations not specified here.
keepParam :: KeepPolicy -> CommandParam
keepParam (KeepHours n) = Param ("--keep-hourly=" ++ val n)
keepParam (KeepDays n) = Param ("--keep-daily=" ++ val n)
keepParam (KeepWeeks n) = Param ("--keep-daily=" ++ val n)
keepParam (KeepMonths n) = Param ("--keep-monthly=" ++ val n)
keepParam (KeepYears n) = Param ("--keep-yearly=" ++ val n)

-- | Policy for backup generations to keep. For example, KeepDays 30 will
-- keep the latest backup for each day when a backup was made, and keep the
-- last 30 such backups. When multiple KeepPolicies are combined together,
-- backups meeting any policy are kept. See borg's man page for details.
data KeepPolicy
	= KeepHours Int
	| KeepDays Int
	| KeepWeeks Int
	| KeepMonths Int
	| KeepYears Int

-- | Construct the encryption type parameter.
encParam :: BorgEnc -> CommandParam
encParam BorgEncNone = Param "--encryption=none"
encParam BorgEncAuthenticated = Param "--encryption=authenticated"
encParam BorgEncAuthenticatedBlake2 = Param "--encryption=authenticated-blake2"
encParam BorgEncRepokey = Param "--encryption=repokey"
encParam BorgEncRepokeyBlake2 = Param "--encryption=repokey-blake2"
encParam BorgEncKeyfile = Param "--encryption=keyfile"
encParam BorgEncKeyfileBlake2 = Param "--encryption=keyfile-blake2"
