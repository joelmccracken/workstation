{-# LANGUAGE PackageImports #-}

-- | This module lets you construct Properties by running commands and
-- scripts. To get from an `UncheckedProperty` to a `Property`, it's
-- up to the user to check if the command made a change to the system. 
--
-- The best approach is to `check` a property, so that the command is only
-- run when it needs to be. With this method, you avoid running the
-- `cmdProperty` unnecessarily.
--
-- > check (not <$> userExists "bob")
-- > 	(cmdProperty "useradd" ["bob"])
--
-- Sometimes it's just as expensive to check a property as it would be to
-- run the command that ensures the property. So you can let the command
-- run every time, and use `changesFile` or `checkResult` to determine if
-- anything changed:
--
-- > cmdProperty "chmod" ["600", "/etc/secret"]
-- > 	`changesFile` "/etc/secret"
--
-- Or you can punt and `assume` a change was made, but then propellor will
-- always say it make a change, and `onChange` will always fire.
--
-- > cmdProperty "service" ["foo", "reload"]
-- > 	`assume` MadeChange

module Propellor.Property.Cmd (
	-- * Constricting properties running commands and scripts
	cmdProperty,
	cmdProperty',
	cmdPropertyEnv,
	Script,
	scriptProperty,
	userScriptProperty,
	cmdResult,
	-- * Lower-level interface for running commands
	CommandParam(..),
	boolSystem,
	boolSystemEnv,
	safeSystem,
	safeSystemEnv,
	shellEscape,
	createProcess,
	waitForProcess,
) where

import Data.List
import "mtl" Control.Monad.Reader
import Control.Applicative
import Prelude

import Propellor.Types
import Propellor.Property
import Utility.SafeCommand
import Utility.Env
import Utility.Process (createProcess, CreateProcess, waitForProcess)

-- | A property that can be satisfied by running a command.
--
-- The command must exit 0 on success.
cmdProperty :: String -> [String] -> UncheckedProperty UnixLike
cmdProperty cmd params = cmdProperty' cmd params id

cmdProperty' :: String -> [String] -> (CreateProcess -> CreateProcess) -> UncheckedProperty UnixLike
cmdProperty' cmd params mkprocess = unchecked $ property desc $ liftIO $
	cmdResult <$> boolSystem' cmd (map Param params) mkprocess
  where
	desc = unwords $ cmd : params

cmdResult :: Bool -> Result
cmdResult False = FailedChange
cmdResult True = NoChange

-- | A property that can be satisfied by running a command,
-- with added environment variables in addition to the standard
-- environment.
cmdPropertyEnv :: String -> [String] -> [(String, String)] -> UncheckedProperty UnixLike
cmdPropertyEnv cmd params env = unchecked $ property desc $ liftIO $ do
	env' <- addEntries env <$> getEnvironment
	cmdResult <$> boolSystemEnv cmd (map Param params) (Just env')
  where
	desc = unwords $ cmd : params

-- | A series of shell commands. (Without a leading hashbang.)
type Script = [String]

-- | A property that can be satisfied by running a script.
scriptProperty :: Script -> UncheckedProperty UnixLike
scriptProperty script = cmdProperty "sh" ["-c", shellcmd]
  where
	shellcmd = intercalate " ; " ("set -e" : script)

-- | A property that can satisfied by running a script
-- as user (cd'd to their home directory).
userScriptProperty :: User -> Script -> UncheckedProperty UnixLike
userScriptProperty (User user) script = cmdProperty "su"
	["--login", "--shell", "/bin/sh", "-c", shellcmd, user]
  where
	shellcmd = intercalate " ; " ("set -e" : "cd" : script)
