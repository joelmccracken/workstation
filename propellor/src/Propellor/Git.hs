module Propellor.Git where

import Utility.Process
import Utility.Exception
import Utility.Directory
import Utility.Misc
import Utility.PartialPrelude

import Data.Maybe
import Control.Applicative
import Prelude

getCurrentBranch :: IO String
getCurrentBranch = takeWhile (/= '\n')
	<$> readProcess "git" ["symbolic-ref", "--short", "HEAD"]

getCurrentBranchRef :: IO String
getCurrentBranchRef = takeWhile (/= '\n')
	<$> readProcess "git" ["symbolic-ref", "HEAD"]

getCurrentGitSha1 :: String -> IO String
getCurrentGitSha1 branchref = takeWhile (/= '\n')
	<$> readProcess "git" ["show-ref", "--hash", branchref]

hasOrigin :: IO Bool
hasOrigin = hasRemote "origin"

hasRemote :: String -> IO Bool
hasRemote remotename = catchDefaultIO False $ do
	rs <- lines <$> readProcess "git" ["remote"]
	return $ remotename `elem` rs

remoteUrl :: String -> IO (Maybe String)
remoteUrl remotename = catchDefaultIO Nothing $ headMaybe . lines
	<$> readProcess "git" ["config", "remote." ++ remotename ++ ".url"]

hasGitRepo :: IO Bool
hasGitRepo = doesFileExist ".git/HEAD"

type Version = [Int]

gitVersion :: IO Version
gitVersion = extract <$> readProcess "git" ["--version"]
  where
	extract s = case lines s of
		[] -> []
		(l:_) -> mapMaybe readish $ segment (== '.') $
			unwords $ drop 2 $ words l
