module Propellor.Debug where

import Control.Monad.IfElse
import System.IO
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import Control.Applicative
import Prelude

import Utility.Monad
import Utility.Env
import Utility.Exception
import Utility.Process
import Utility.Directory

debug :: [String] -> IO ()
debug = debugM "propellor" . unwords

checkDebugMode :: IO ()
checkDebugMode = go =<< getEnv "PROPELLOR_DEBUG"
  where
	go (Just "1") = enableDebugMode
	go (Just _) = noop
	go Nothing = whenM (doesDirectoryExist ".git") $
		whenM (elem "1" . lines <$> getgitconfig) enableDebugMode
	getgitconfig = catchDefaultIO "" $
		readProcess "git" ["config", "propellor.debug"]

enableDebugMode :: IO ()
enableDebugMode = do
	f <- setFormatter
		<$> streamHandler stderr DEBUG
		<*> pure (simpleLogFormatter "[$time] $msg")
	updateGlobalLogger rootLoggerName $ 
		setLevel DEBUG .  setHandlers [f]
