-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>
--
-- Support for the Attic backup tool <https://attic-backup.org/>
--
-- This module is deprecated because Attic is not available in debian
-- stable any longer (so the installed property no longer works), and it
-- appears to have been mostly supersceded by Borg.

module Propellor.Property.Attic {-# DEPRECATED "Use Borg instead" #-}
	( installed
	, repoExists
	, init
	, restored
	, backup
	, KeepPolicy (..)
	) where

import Propellor.Base hiding (init)
import Prelude hiding (init)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Data.List (intercalate)

type AtticParam = String

type AtticRepo = FilePath

installed :: Property DebianLike
installed = Apt.installed ["attic"]

repoExists :: AtticRepo -> IO Bool
repoExists repo = boolSystem "attic" [Param "list", File repo]

-- | Inits a new attic repository
init :: AtticRepo -> Property DebianLike
init backupdir = check (not <$> repoExists backupdir) (cmdProperty "attic" initargs)
	`requires` installed
  where
	initargs =
		[ "init"
		, backupdir
		]

-- | Restores a directory from an attic backup.
--
-- Only does anything if the directory does not exist, or exists,
-- but is completely empty.
--
-- The restore is performed atomically; restoring to a temp directory
-- and then moving it to the directory.
restored :: FilePath -> AtticRepo -> Property DebianLike
restored dir backupdir = go `requires` installed
  where
	go :: Property DebianLike
	go = property (dir ++ " restored by attic") $ ifM (liftIO needsRestore)
		( do
			warningMessage $ dir ++ " is empty/missing; restoring from backup ..."
			liftIO restore
		, noChange
		)

	needsRestore = isUnpopulated dir

	restore = withTmpDirIn (takeDirectory dir) "attic-restore" $ \tmpdir -> do
		ok <- boolSystem "attic" $
			[ Param "extract"
			, Param backupdir
			, Param tmpdir
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
-- up, by running attic with some parameters.
--
-- If the directory does not exist, or exists but is completely empty,
-- this Property will immediately restore it from an existing backup.
--
-- So, this property can be used to deploy a directory of content
-- to a host, while also ensuring any changes made to it get backed up.
-- For example:
--
-- >	& Attic.backup "/srv/git" "root@myserver:/mnt/backup/git.attic" Cron.Daily
-- >		["--exclude=/srv/git/tobeignored"]
-- >		[Attic.KeepDays 7, Attic.KeepWeeks 4, Attic.KeepMonths 6, Attic.KeepYears 1]
--
-- Note that this property does not make attic encrypt the backup
-- repository.
--
-- Since attic uses a fair amount of system resources, only one attic
-- backup job will be run at a time. Other jobs will wait their turns to
-- run.
backup :: FilePath -> AtticRepo -> Cron.Times -> [AtticParam] -> [KeepPolicy] -> Property DebianLike
backup dir backupdir crontimes extraargs kp = backup' dir backupdir crontimes extraargs kp
	`requires` restored dir backupdir

-- | Does a backup, but does not automatically restore.
backup' :: FilePath -> AtticRepo -> Cron.Times -> [AtticParam] -> [KeepPolicy] -> Property DebianLike
backup' dir backupdir crontimes extraargs kp = cronjob
	`describe` desc
	`requires` installed
  where
	desc = backupdir ++ " attic backup"
	cronjob = Cron.niceJob ("attic_backup" ++ dir) crontimes (User "root") "/" $
		"flock " ++ shellEscape lockfile ++ " sh -c " ++ shellEscape backupcmd
	lockfile = "/var/lock/propellor-attic.lock"
	backupcmd = intercalate ";" $
		createCommand
		: if null kp then [] else [pruneCommand]
	createCommand = unwords $
		[ "attic"
		, "create"
		, "--stats"
		]
		++ map shellEscape extraargs ++
		[ shellEscape backupdir ++ "::" ++ "$(date --iso-8601=ns --utc)"
		, shellEscape dir
		]
	pruneCommand = unwords $
		[ "attic"
		, "prune"
		, shellEscape backupdir
		]
		++
		map keepParam kp

-- | Constructs an AtticParam that specifies which old backup generations to
-- keep. By default, all generations are kept. However, when this parameter is
-- passed to the `backup` property, they will run attic prune to clean out
-- generations not specified here.
keepParam :: KeepPolicy -> AtticParam
keepParam (KeepHours n) = "--keep-hourly=" ++ val n
keepParam (KeepDays n) = "--keep-daily=" ++ val n
keepParam (KeepWeeks n) = "--keep-daily=" ++ val n
keepParam (KeepMonths n) = "--keep-monthly=" ++ val n
keepParam (KeepYears n) = "--keep-yearly=" ++ val n

-- | Policy for backup generations to keep. For example, KeepDays 30 will
-- keep the latest backup for each day when a backup was made, and keep the
-- last 30 such backups. When multiple KeepPolicies are combined together,
-- backups meeting any policy are kept. See attic's man page for details.
data KeepPolicy
	= KeepHours Int
	| KeepDays Int
	| KeepWeeks Int
	| KeepMonths Int
	| KeepYears Int
