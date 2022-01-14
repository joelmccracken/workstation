-- | Support for the Obnam backup tool <http://obnam.org/>
--
-- This module is deprecated because Obnam has been retired by its
-- author.

module Propellor.Property.Obnam {-# DEPRECATED "Obnam has been retired; time to transition to something else" #-} where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Gpg as Gpg

import Data.List

type ObnamParam = String

-- | An obnam repository can be used by multiple clients. Obnam uses
-- locking to allow only one client to write at a time. Since stale lock
-- files can prevent backups from happening, it's more robust, if you know
-- a repository has only one client, to force the lock before starting a
-- backup. Using OnlyClient allows propellor to do so when running obnam.
data NumClients = OnlyClient | MultipleClients
	deriving (Eq)

-- | Installs a cron job that causes a given directory to be backed
-- up, by running obnam with some parameters.
--
-- If the directory does not exist, or exists but is completely empty,
-- this Property will immediately restore it from an existing backup.
--
-- So, this property can be used to deploy a directory of content
-- to a host, while also ensuring any changes made to it get backed up.
-- For example: 
--
-- >	& Obnam.backup "/srv/git" "33 3 * * *"
-- >		[ "--repository=sftp://2318@usw-s002.rsync.net/~/mygitrepos.obnam"
-- >		] Obnam.OnlyClient
-- >		`requires` Ssh.keyImported SshEd25519 "root" (Context hostname)
--
-- How awesome is that?
--
-- Note that this property does not make obnam encrypt the backup
-- repository.
--
-- Since obnam uses a fair amount of system resources, only one obnam
-- backup job will be run at a time. Other jobs will wait their turns to
-- run.
backup :: FilePath -> Cron.Times -> [ObnamParam] -> NumClients -> Property DebianLike
backup dir crontimes params numclients =
	backup' dir crontimes params numclients
		`requires` restored dir params

-- | Like backup, but the specified gpg key id is used to encrypt
-- the repository.
--
-- The gpg secret key will be automatically imported
-- into root's keyring using Propellor.Property.Gpg.keyImported
backupEncrypted :: FilePath -> Cron.Times -> [ObnamParam] -> NumClients -> Gpg.GpgKeyId -> Property (HasInfo + DebianLike)
backupEncrypted dir crontimes params numclients keyid =
	backup dir crontimes params' numclients
		`requires` Gpg.keyImported keyid (User "root")
  where
	params' = ("--encrypt-with=" ++ Gpg.getGpgKeyId keyid) : params

-- | Does a backup, but does not automatically restore.
backup' :: FilePath -> Cron.Times -> [ObnamParam] -> NumClients -> Property DebianLike
backup' dir crontimes params numclients = cronjob `describe` desc
  where
	desc = dir ++ " backed up by obnam"
	cronjob = Cron.niceJob ("obnam_backup" ++ dir) crontimes (User "root") "/" $
		"flock " ++ shellEscape lockfile ++ " sh -c " ++ shellEscape cmdline
	lockfile = "/var/lock/propellor-obnam.lock"
	cmdline = unwords $ catMaybes
		[ if numclients == OnlyClient
			-- forcelock fails if repo does not exist yet
			then Just $ forcelockcmd ++ " 2>/dev/null ;"
			else Nothing
		, Just backupcmd
		, if any isKeepParam params
			then Just $ "&& " ++ forgetcmd
			else Nothing
		]
	forcelockcmd = unwords $
		[ "obnam"
		, "force-lock"
		] ++ map shellEscape params
	backupcmd = unwords $
		[ "obnam"
		, "backup"
		, shellEscape dir
		] ++ map shellEscape params
	forgetcmd = unwords $
		[ "obnam"
		, "forget"
		] ++ map shellEscape params

-- | Restores a directory from an obnam backup.
--
-- Only does anything if the directory does not exist, or exists,
-- but is completely empty.
--
-- The restore is performed atomically; restoring to a temp directory
-- and then moving it to the directory.
restored :: FilePath -> [ObnamParam] -> Property DebianLike
restored dir params = go `requires` installed
  where
	desc = dir ++ " restored by obnam"
	go :: Property DebianLike
	go = property desc $ ifM (liftIO needsRestore)
		( do
			warningMessage $ dir ++ " is empty/missing; restoring from backup ..."
			liftIO restore
		, noChange
		)

	needsRestore = isUnpopulated dir

	restore = withTmpDirIn (takeDirectory dir) "obnam-restore" $ \tmpdir -> do
		ok <- boolSystem "obnam" $
			[ Param "restore"
			, Param "--to"
			, Param tmpdir
			] ++ map Param params
		let restoreddir = tmpdir ++ "/" ++ dir
		ifM (pure ok <&&> doesDirectoryExist restoreddir)
			( do
				void $ tryIO $ removeDirectory dir
				renameDirectory restoreddir dir
				return MadeChange
			, return FailedChange
			)

-- | Policy for backup generations to keep. For example, KeepDays 30 will
-- keep the latest backup for each day when a backup was made, and keep the
-- last 30 such backups. When multiple KeepPolicies are combined together,
-- backups meeting any policy are kept. See obnam's man page for details.
data KeepPolicy 
	= KeepHours Int
	| KeepDays Int
	| KeepWeeks Int
	| KeepMonths Int
	| KeepYears Int

-- | Constructs an ObnamParam that specifies which old backup generations
-- to keep. By default, all generations are kept. However, when this parameter
-- is passed to the `backup` or `backupEncrypted` properties, they will run
-- obnam forget to clean out generations not specified here.
keepParam :: [KeepPolicy] -> ObnamParam
keepParam ps = "--keep=" ++ intercalate "," (map go ps)
  where
	go (KeepHours n) = mk n 'h'
	go (KeepDays n) = mk n 'd'
	go (KeepWeeks n) = mk n 'w'
	go (KeepMonths n) = mk n 'm'
	go (KeepYears n) = mk n 'y'
	mk n c = val n ++ [c]

isKeepParam :: ObnamParam -> Bool
isKeepParam p = "--keep=" `isPrefixOf` p

installed :: Property DebianLike
installed = Apt.installed ["obnam"]
