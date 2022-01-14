{- Running processes in the foreground, not via the concurrent-output
 - layer.
 -
 - Avoid using this in propellor properties!
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Process.NonConcurrent where

import System.Process
import System.Exit
import System.IO
import Utility.SafeCommand
import Control.Applicative
import Prelude

boolSystemNonConcurrent :: String -> [CommandParam] -> IO Bool
boolSystemNonConcurrent cmd params = do
	(Nothing, Nothing, Nothing, p) <- createProcessNonConcurrent $
		proc cmd (toCommand params)
	dispatch <$> waitForProcessNonConcurrent p
  where
	dispatch ExitSuccess = True
	dispatch _ = False

createProcessNonConcurrent :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcessNonConcurrent = createProcess

waitForProcessNonConcurrent  :: ProcessHandle -> IO ExitCode
waitForProcessNonConcurrent  = waitForProcess
