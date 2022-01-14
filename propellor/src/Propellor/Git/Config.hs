module Propellor.Git.Config where

import Propellor.Git
import Utility.Process
import Utility.Exception
import Utility.SafeCommand
import Utility.Monad

import Control.Monad
import Control.Applicative
import Prelude

getGitConfigValue :: String -> IO (Maybe String)
getGitConfigValue key = do
	value <- catchMaybeIO $
		takeWhile (/= '\n')
			<$> readProcess"git" ["config", key]
	return $ case value of
		Just v | not (null v) -> Just v
		_ -> Nothing

-- `git config --bool propellor.blah` outputs "false" if propellor.blah is unset
-- i.e. the git convention is that the default value of any git-config setting
-- is "false".  So we don't need a Maybe Bool here.
getGitConfigBool :: String -> IO Bool
getGitConfigBool key = do
	value <- catchMaybeIO $
		takeWhile (/= '\n')
			<$> readProcess "git" ["config", "--bool", key]
	return $ case value of
		Just "true" -> True
		_ -> False

setRepoUrl :: String -> IO ()
setRepoUrl "" = return ()
setRepoUrl url = do
	subcmd <- ifM hasOrigin (pure "set-url", pure "add")
	void $ boolSystem "git" [Param "remote", Param subcmd, Param "origin", Param url]
	-- same as --set-upstream-to, except origin branch
	-- may not have been pulled yet
	branch <- getCurrentBranch
	let branchval s = "branch." ++ branch ++ "." ++ s
	void $ boolSystem "git" [Param "config", Param (branchval "remote"), Param "origin"]
	void $ boolSystem "git" [Param "config", Param (branchval "merge"), Param $ "refs/heads/"++branch]

getRepoUrl :: IO (Maybe String)
getRepoUrl = getM getGitConfigValue urls
  where
	urls = ["remote.deploy.url", "remote.origin.url"]
