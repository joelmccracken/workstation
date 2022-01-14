-- | This module is used to implement a wrapper program for propellor
-- distribution.
--
-- Distributions should install this program into PATH.
--
-- This is not the propellor main program (that's config.hs).
-- This bootstraps ~/.propellor/config.hs, builds it if
-- it's not already built, and runs it.
--
-- If ./config.hs exists and looks like a propellor config file, 
-- it instead builds and runs in the current working directory.

module Propellor.Wrapper (runWrapper) where

import Propellor.DotDir
import Propellor.Message
import Propellor.Bootstrap
import Utility.Monad
import Utility.Directory
import Utility.FileMode
import Utility.Process
import Utility.Process.NonConcurrent
import Utility.FileSystemEncoding

import System.Environment (getArgs)
import System.Exit
import System.Posix
import Data.List
import Control.Monad.IfElse
import Control.Applicative
import Prelude

runWrapper :: IO ()
runWrapper = withConcurrentOutput $ do
	useFileSystemEncoding
	go =<< getArgs
  where
	go ["--init"] = interactiveInit
	go args = ifM configInCurrentWorkingDirectory
		( buildRunConfig args
		, ifM (doesDirectoryExist =<< dotPropellor)
			( do
				checkRepoUpToDate
				changeWorkingDirectory =<< dotPropellor
				buildRunConfig args
			, error "Seems that ~/.propellor/ does not exist. To set it up, run: propellor --init"
			)
		)

buildRunConfig :: [String] -> IO ()
buildRunConfig args = do
	unlessM (doesFileExist "propellor") $ do
		buildPropellor Nothing
		putStrLn ""
		putStrLn ""
	(_, _, _, pid) <- createProcessNonConcurrent (proc "./propellor" args) 
	exitWith =<< waitForProcessNonConcurrent pid

configInCurrentWorkingDirectory :: IO Bool
configInCurrentWorkingDirectory = ifM (doesFileExist "config.hs")
	( do
		-- This is a security check to avoid using the current
		-- working directory as the propellor configuration
		-- if it's not owned by the user, or is world-writable,
		-- or group writable. (Some umasks may make directories
		-- group writable, but typical ones do not.)
		s <- getFileStatus "."
		uid <- getRealUserID
		if fileOwner s /= uid
			then unsafe "you don't own the current directory"
			else if checkMode groupWriteMode (fileMode s)
				then unsafe "the current directory is group writable"
				else if checkMode otherWriteMode (fileMode s)
					then unsafe "the current directory is world-writable"
					else ifM mentionspropellor
						( return True
						, notusing "it does not seem to be a propellor config file"
						)
	, return False
	)
  where
	unsafe s = notusing (s ++ ". This seems unsafe.")
	notusing s = error $ "Not using ./config.hs because " ++ s
	mentionspropellor = ("Propellor" `isInfixOf`) <$> readFile "config.hs"
