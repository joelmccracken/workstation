{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Text (unpack)

data Command = Check | Install
  deriving (Show)

parser :: Parser Command
parser =   fmap (const Check)   (subcommand "check" "run system tests" $ pure ())
       <|> fmap (const Install) (subcommand "install" "install anything it can" $ pure ())

main :: IO ()
main = do
  theOption <- options "workstation checker" parser
  case theOption of
    Check -> check
    Install -> install

check :: IO ()
check = sh $ do
  file <- ls "/Users/jmccracken/check"
  liftIO $ print $ format fp file
  exitCode <- shell (format fp file) empty
  echo $ fromString $ "exit: " ++ show exitCode

install :: IO ()
install = sh $ do
  file <- ls "/Users/jmccracken/install"
  liftIO $ putStrLn $ "Executing: " ++ (unpack $ format fp file)
  exitCode <- shell (format fp file) empty
  liftIO $ putStrLn $ (unpack $ format fp file) <> (unpack ": ") <> ((unpack "exit: ") ++ show exitCode)



-- Local Variables: ***
-- mode:haskell ***
-- End: ***
