-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>
--
-- Support for the restic backup tool <https://github.com/restic/restic>

module Propellor.Property.Restic
	( ResticRepo (..)
	, installed
	, repoExists
	, init
	, restored
	, backup
	, backup'
	, KeepPolicy (..)
	) where

import Propellor.Base hiding (init)
import Prelude hiding (init)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import Data.List (intercalate)

type Url = String

type ResticParam = String

data ResticRepo
	= Direct FilePath
	| SFTP User HostName FilePath
	| REST Url

instance ConfigurableValue ResticRepo where
	val (Direct fp) = fp
	val (SFTP u h fp) = "sftp:" ++ val u ++ "@" ++ val h ++ ":" ++ fp
	val (REST url) = "rest:" ++ url

installed :: Property DebianLike
installed = Apt.installed ["restic"] `describe` "installed restic"

repoExists :: ResticRepo -> IO Bool
repoExists repo = boolSystem "restic"
	[ Param "-r"
	, File (val repo)
	, Param "--password-file"
	, File (getPasswordFile repo)
	, Param "snapshots"
	]

passwordFileDir :: FilePath
passwordFileDir = "/etc/restic-keys"

getPasswordFile :: ResticRepo -> FilePath
getPasswordFile repo = passwordFileDir </> File.configFileName (val repo)

passwordFileConfigured :: ResticRepo -> Property (HasInfo + UnixLike)
passwordFileConfigured repo = propertyList "restic password file" $ props
	& File.dirExists passwordFileDir
	& File.mode passwordFileDir 0O2700
	& getPasswordFile repo `File.hasPrivContent` hostContext

-- | Inits a new restic repository
init :: ResticRepo -> Property (HasInfo + DebianLike)
init repo = check (not <$> repoExists repo) (cmdProperty "restic" initargs)
	`requires` installed
	`requires` passwordFileConfigured repo
  where
	initargs =
		[ "-r"
		, val repo
		, "--password-file"
		, getPasswordFile repo
		, "init"
		]

-- | Restores a directory from a restic backup.
--
-- Only does anything if the directory does not exist, or exists,
-- but is completely empty.
--
-- The restore is performed atomically; restoring to a temp directory
-- and then moving it to the directory.
restored :: FilePath -> ResticRepo -> Property (HasInfo + DebianLike)
restored dir repo = go
	`requires` init repo
  where
	go :: Property DebianLike
	go = property (dir ++ " restored by restic") $ ifM (liftIO needsRestore)
		( do
			warningMessage $ dir ++ " is empty/missing; restoring from backup ..."
			liftIO restore
		, noChange
		)

	needsRestore = isUnpopulated dir

	restore = withTmpDirIn (takeDirectory dir) "restic-restore" $ \tmpdir -> do
		ok <- boolSystem "restic"
			[ Param "-r"
			, File (val repo)
			, Param "--password-file"
			, File (getPasswordFile repo)
			, Param "restore"
			, Param "latest"
			, Param "--target"
			, File tmpdir
			]
		let restoreddir = tmpdir ++ "/" ++ dir
		ifM (pure ok <&&> doesDirectoryExist restoreddir)
			( do
				void $ tryIO $ removeDirectory dir
				renameDirectory restoreddir dir
				return MadeChange
			, return FailedChange
			)

-- | Installs a cron job that causes a given directory to be backed
-- up, by running restic with some parameters.
--
-- If the directory does not exist, or exists but is completely empty,
-- this Property will immediately restore it from an existing backup.
--
-- So, this property can be used to deploy a directory of content
-- to a host, while also ensuring any changes made to it get backed up.
-- For example:
--
-- >	& Restic.backup "/srv/git"
-- >		(Restic.SFTP (User root) (HostName myserver) /mnt/backup/git.restic")
-- >		Cron.Daily
-- >		["--exclude=/srv/git/tobeignored"]
-- >		[Restic.KeepDays 7, Restic.KeepWeeks 4, Restic.KeepMonths 6, Restic.KeepYears 1]
--
-- Since restic uses a fair amount of system resources, only one restic
-- backup job will be run at a time. Other jobs will wait their turns to
-- run.
backup :: FilePath -> ResticRepo -> Cron.Times -> [ResticParam] -> [KeepPolicy] -> Property (HasInfo + DebianLike)
backup dir repo crontimes extraargs kp = backup' [dir] repo crontimes extraargs kp
	`requires` restored dir repo

-- | Does a backup, but does not automatically restore.
backup' :: [FilePath] -> ResticRepo -> Cron.Times -> [ResticParam] -> [KeepPolicy] -> Property (HasInfo + DebianLike)
backup' dirs repo crontimes extraargs kp = cronjob
	`describe` desc
	`requires` init repo
  where
	desc = val repo ++ " restic backup"
	cronjob = Cron.niceJob ("restic_backup" ++ intercalate "_" dirs) crontimes (User "root") "/" $
		"flock " ++ shellEscape lockfile ++ " sh -c " ++ shellEscape backupcmd
	lockfile = "/var/lock/propellor-restic.lock"
	backupcmd = intercalate " && " $
		createCommand
		: if null kp then [] else [pruneCommand]
	createCommand = unwords $
		[ "restic"
		, "-r"
		, shellEscape (val repo)
		, "--password-file"
		, shellEscape (getPasswordFile repo)
		]
		++ map shellEscape extraargs ++
		[ "backup" ]
		++ map shellEscape dirs
	pruneCommand = unwords $
		[ "restic"
		, "-r"
		, shellEscape (val repo)
		, "--password-file"
		, shellEscape (getPasswordFile repo)
		, "forget"
		, "--prune"
		]
		++
		map keepParam kp

-- | Constructs a ResticParam that specifies which old backup generations to
-- keep. By default, all generations are kept. However, when this parameter is
-- passed to the `backup` property, they will run restic prune to clean out
-- generations not specified here.
keepParam :: KeepPolicy -> ResticParam
keepParam (KeepLast n) = "--keep-last=" ++ val n
keepParam (KeepHours n) = "--keep-hourly=" ++ val n
keepParam (KeepDays n) = "--keep-daily=" ++ val n
keepParam (KeepWeeks n) = "--keep-weekly=" ++ val n
keepParam (KeepMonths n) = "--keep-monthly=" ++ val n
keepParam (KeepYears n) = "--keep-yearly=" ++ val n

-- | Policy for backup generations to keep. For example, KeepDays 30 will
-- keep the latest backup for each day when a backup was made, and keep the
-- last 30 such backups. When multiple KeepPolicies are combined together,
-- backups meeting any policy are kept. See restic's man page for details.
data KeepPolicy
	= KeepLast Int
	| KeepHours Int
	| KeepDays Int
	| KeepWeeks Int
	| KeepMonths Int
	| KeepYears Int
