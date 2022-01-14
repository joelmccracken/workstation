-- | 
-- Copyright: 2015 Joey Hess <id@joeyh.name>
-- License: BSD-2-clause
-- 
-- The functions exported by this module are intended to be drop-in
-- replacements for those from System.Process, when converting a whole
-- program to use System.Console.Concurrent.

module System.Process.Concurrent where

import System.Console.Concurrent
import System.Console.Concurrent.Internal (ConcurrentProcessHandle(..))
import System.Process hiding (createProcess, waitForProcess)
import System.IO
import System.Exit

-- | Calls `createProcessConcurrent`
--
-- You should use the waitForProcess in this module on the resulting
-- ProcessHandle. Using System.Process.waitForProcess instead can have
-- mildly unexpected results.
createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess p = do
	(i, o, e, ConcurrentProcessHandle h) <- createProcessConcurrent p
	return (i, o, e, h)

-- | Calls `waitForProcessConcurrent`
--
-- You should only use this on a ProcessHandle obtained by calling
-- createProcess from this module. Using this with a ProcessHandle
-- obtained from System.Process.createProcess etc will have extremely
-- unexpected results; it can wait a very long time before returning.
waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess = waitForProcessConcurrent . ConcurrentProcessHandle
