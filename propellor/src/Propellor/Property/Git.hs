module Propellor.Property.Git where

import Propellor.Base
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

import Data.List

-- | Exports all git repos in a directory (that user nobody can read)
-- using git-daemon, run from inetd.
--
-- Note that reverting this property does not remove or stop inetd.
daemonRunning :: FilePath -> RevertableProperty DebianLike DebianLike
daemonRunning exportdir = setup <!> unsetup
  where
	setup = containsLine conf (mkl "tcp4")
		`requires`
		containsLine conf (mkl "tcp6")
		`requires`
		dirExists exportdir
		`requires`
		Apt.serviceInstalledRunning "openbsd-inetd"
		`onChange`
		Service.reloaded "openbsd-inetd"
		`describe` ("git-daemon exporting " ++ exportdir)
	unsetup = lacksLine conf (mkl "tcp4")
		`requires`
		lacksLine conf (mkl "tcp6")
		`onChange`
		Service.reloaded "openbsd-inetd"

	conf = "/etc/inetd.conf"

	mkl tcpv = intercalate "\t"
		[ "git"
		, "stream"
		, tcpv
		, "nowait"
		, "nobody"
		, "/usr/bin/git"
		, "git"
		, "daemon"
		, "--inetd"
		, "--export-all"
		, "--base-path=" ++ exportdir
		, exportdir
		]

installed :: Property DebianLike
installed = Apt.installed ["git"]

type RepoUrl = String

type Branch = String

-- | Specified git repository is cloned to the specified directory.
--
-- If the directory exists with some other content (either a non-git
-- repository, or a git repository cloned from some other location),
-- it will be recursively deleted first.
--
-- A branch can be specified, to check out.
--
-- Does not make subsequent changes be pulled into the repository after
-- it's cloned.
cloned :: User -> RepoUrl -> FilePath -> Maybe Branch -> Property DebianLike
cloned owner url dir mbranch = check originurl go
	`requires` installed
  where
	desc = "git cloned " ++ url ++ " to " ++ dir
	gitconfig = dir </> ".git/config"
	originurl = ifM (doesFileExist gitconfig)
		( do
			v <- catchDefaultIO Nothing $ headMaybe . lines <$>
				readProcess "git" ["config", "--file", gitconfig, "remote.origin.url"]
			return (v /= Just url)
		, return True
		)
	go :: Property DebianLike
	go = property' desc $ \w -> do
		liftIO $ do
			whenM (doesDirectoryExist dir) $
				removeDirectoryRecursive dir
			createDirectoryIfMissing True (takeDirectory dir)
		ensureProperty w $ userScriptProperty owner (catMaybes checkoutcmds)
			`assume` MadeChange
	checkoutcmds = 
		-- The </dev/null fixes an intermittent
		-- "fatal: read error: Bad file descriptor"
		-- when run across ssh with propellor --spin
		[ Just $ "git clone " ++ shellEscape url ++ " " ++ shellEscape dir ++ " < /dev/null"
		, Just $ "cd " ++ shellEscape dir
		, ("git checkout " ++) <$> mbranch
		-- In case this repo is exposted via the web,
		-- although the hook to do this ongoing is not
		-- installed here.
		, Just "git update-server-info"
		]

-- | Specified git repository is cloned to the specified directory,
-- and any new commits are pulled into it each time this property runs.
pulled :: User -> RepoUrl -> FilePath -> Maybe Branch -> Property DebianLike
pulled owner url dir mbranch = go
	`requires` cloned owner url dir mbranch
	`describe` desc
  where
	desc = "git pulled " ++ url ++ " to " ++ dir
	go = userScriptProperty owner
		[ "cd " ++ shellEscape dir
		, "git pull"
		]
		`changesFileContent` (dir </> ".git" </> "FETCH_HEAD")

isGitDir :: FilePath -> IO Bool
isGitDir dir = isNothing <$> catchMaybeIO (readProcess "git" ["rev-parse", "--resolve-git-dir", dir])

data GitShared = Shared Group | SharedAll | NotShared

-- | Sets up a new, empty bare git repository.
bareRepo :: FilePath -> User -> GitShared -> Property UnixLike
bareRepo repo user gitshared = check (isRepo repo) $ propertyList ("git repo: " ++ repo) $ toProps $
	dirExists repo : case gitshared of
		NotShared ->
			[ ownerGroup repo user (userGroup user)
			, userScriptProperty user ["git init --bare --shared=false " ++ shellEscape repo]
				`assume` MadeChange
			]
		SharedAll ->
			[ ownerGroup repo user (userGroup user)
			, userScriptProperty user ["git init --bare --shared=all " ++ shellEscape repo]
				`assume` MadeChange
			]
		Shared group' ->
			[ ownerGroup repo user group'
			, userScriptProperty user ["git init --bare --shared=group " ++ shellEscape repo]
				`assume` MadeChange
			]
  where
	isRepo repo' = isNothing <$> catchMaybeIO (readProcess "git" ["rev-parse", "--resolve-git-dir", repo'])

-- | Set a key value pair in a git repo's configuration.
repoConfigured :: FilePath -> (String, String) -> Property UnixLike
repo `repoConfigured` (key, value) = check (not <$> alreadyconfigured) $
	userScriptProperty (User "root")
		[ "cd " ++ repo
		, "git config " ++ key ++ " " ++ value
		]
		`assume` MadeChange
		`describe` desc
  where
	alreadyconfigured = do
		vs <- getRepoConfig repo key
		return $ value `elem` vs
	desc = "git repo at " ++ repo  ++ " config setting " ++ key ++ " set to " ++ value

-- | Gets the value that a key is set to in a git repo's configuration.
getRepoConfig :: FilePath -> String -> IO [String]
getRepoConfig repo key = catchDefaultIO [] $
	lines <$> readProcess "git" ["-C", repo, "config", key]

-- | Whether a repo accepts non-fast-forward pushes.
repoAcceptsNonFFs :: FilePath -> RevertableProperty UnixLike UnixLike
repoAcceptsNonFFs repo = accepts <!> refuses
  where
	accepts = repoConfigured repo ("receive.denyNonFastForwards", "false")
		`describe` desc "accepts"
	refuses = repoConfigured repo ("receive.denyNonFastForwards", "true")
		`describe` desc "rejects"
	desc s = "git repo " ++ repo ++ " " ++ s ++ " non-fast-forward pushes"

-- | Sets a bare repository's default branch, which will be checked out
-- when cloning it.
bareRepoDefaultBranch :: FilePath -> String -> Property UnixLike
bareRepoDefaultBranch repo branch =
	userScriptProperty (User "root")
		[ "cd " ++ repo
		, "git symbolic-ref HEAD refs/heads/" ++ branch
		]
	`changesFileContent` (repo </> "HEAD")
	`describe` ("git repo at " ++ repo ++ " has default branch " ++ branch)
