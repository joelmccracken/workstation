{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Optional
import Data.Text (unpack)

data Options =
  Options { machineName :: Text
          , command :: Command
          }
  deriving (Eq, Show)

data Command = Check | Install
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions =
  Options <$> argText "MACHINENAME" Default <*> commandParser

commandParser :: Parser Command
commandParser =
  fmap (const Check)   (subcommand "check" "run system tests" $ pure ())
  <|> fmap (const Install) (subcommand "install" "install anything it can" $ pure ())

{-
- run with file handles; a fully custom, interactive
- run to completion, capturing output

- run command over ssh
- run command in a shell script(that is, generate a string, write it to /tmp, execute)

- define interfaces to external processes; create library of them
-}

main :: IO ()
main = do
  theOption <- options "workstation manager" parseOptions
  case command theOption of
    -- Check -> check
    Install -> install

-- sh' :: Shell a -> IO [a]
-- sh' s = fold s (Fold (flip (:)) [] id)

-- check :: IO ()
-- check = do
--   file <- sh' (ls "/Users/jmccracken/check")
--   liftIO $ print $ format fp (head file)
--   exitCode <- shell (format fp (head file)) empty
--   echo $ fromString $ "exit: " ++ show exitCode

install :: IO ()
install = sh $ do
  file <- ls "/Users/jmccracken/install"
  liftIO $ putStrLn $ "Executing: " ++ (unpack $ format fp file)
  exitCode <- shell (format fp file) empty
  liftIO $ putStrLn $ (unpack $ format fp file) <> (unpack ": ") <> ((unpack "exit: ") ++ show exitCode)



-- Local Variables: ***
-- mode:haskell ***
-- End: ***
