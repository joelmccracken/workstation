{- testing properties of directories
 -
 - Copyright 2011-2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Directory.TestDirectory where

import Utility.Directory
import Utility.Directory.Stream
import Utility.Exception

-- | True only when directory exists and contains nothing.
-- Throws exception if directory does not exist.
isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty d = testDirectory d dirCruft

-- | True if the directory does not exist or contains nothing.
-- Ignores "lost+found" which can exist in an empty filesystem.
isUnpopulated :: FilePath -> IO Bool
isUnpopulated d = catchDefaultIO True $ testDirectory d fsCruft

fsCruft :: FilePath -> Bool
fsCruft "lost+found" = True
fsCruft d = dirCruft d

-- | Run test on entries found in directory, return False as soon as the
-- test returns False, else return True.  Throws exception if directory does
-- not exist.
testDirectory :: FilePath -> (FilePath -> Bool) -> IO Bool
testDirectory d test = bracket (openDirectory d) closeDirectory check
  where
	check h = do
		v <- readDirectory h
		case v of
			Nothing -> return True
			Just f
				| not (test f) -> return False
				| otherwise -> check h
