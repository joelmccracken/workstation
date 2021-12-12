{-# LANGUAGE OverloadedStrings #-}
import Turtle
import Data.Text (unpack)

data Command = Check | Install | Remove
  deriving (Show)

parser :: Parser Command
parser =   fmap (const Check)   (subcommand "check" "run system tests" $ pure ())
       <|> fmap (const Install) (subcommand "install" "install anything it can" $ pure ())

main = do
  x <- options "workstation checker" parser
  case x of
    Check -> check
    Install -> install

check = sh $ do
  file <- ls "/Users/jmccracken/check"
  liftIO $ print $ format fp file
  exitCode <- shell (format fp file) empty
  echo $ fromString $ "exit: " ++ show exitCode

install = sh $ do
  file <- ls "/Users/jmccracken/install"
  liftIO $ putStrLn $ "Executing: " ++ (unpack $ format fp file)
  exitCode <- shell (format fp file) empty
  liftIO $ putStrLn $ (unpack $ format fp file) <> (unpack ": ") <> ((unpack "exit: ") ++ show exitCode)



-- Local Variables: ***
-- mode:haskell ***
-- End: ***
