import Turtle
import qualified Data.Text as T
import WSHS.Properties.Core
import WSHS.Properties.MacOS

data Options =
  Options { machineName :: Text
          , command :: Command
          }
  deriving (Eq, Show)

data Command = Check | Install
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions =
  Options <$> optText "machine-name" 'm' "the name of this machine" <*> commandParser

commandParser :: Parser Command
commandParser =
  fmap (const Check)   (subcommand "check" "run system tests" $ pure ())
  <|> fmap (const Install) (subcommand "install" "install anything it can" $ pure ())

main :: IO ()
main = do
  Options { .. } <- options "workstation manager" parseOptions
  case command of
    Install -> machineNameToProfile machineName
    Check -> error "check not implemented yet"


machineNameToProfile :: Text -> IO ()
machineNameToProfile name =
  case name of
    "ci-macos" -> ciMacos
    "ci-ubuntu" -> return () -- TODO add this
    _ -> error $ "No configuration available for machine named " <> T.unpack name

-- check :: IO ()
-- check = do
--   file <- sh' (ls "/Users/jmccracken/check")
--   liftIO $ print $ format fp (head file)
--   exitCode <- shell (format fp (head file)) empty
--   echo $ fromString $ "exit: " ++ show exitCode

-- unFilePath :: Turtle.FilePath -> IO Text
-- unFilePath fp =
--   return $ either (error . (unpack . ("could not decode filepath: " <>))) id $ toText fp

ciMacos :: IO ()
ciMacos = do
  satisfyProperties [brewBundled]

