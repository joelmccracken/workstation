{-# LANGUAGE OverloadedStrings #-}

-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import Propellor.Engine
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Cmd as Cmd
import System.FilePath
import Utility.UserInfo
import Control.Monad.IO.Class
import Data.Functor
import Options.Applicative
import qualified Data.Text as T

data Options = Options { machineName :: T.Text }

parseOptions :: Parser Options
parseOptions =
  Options <$> argument str (metavar "MACHINENAME")

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  options <- execParser opts
  let theHost
        = case machineName options of
            "glamdring" -> glamdring
            _ -> error $ T.unpack ("unknown host name " <>  machineName options)

  mainProperties $ theHost

-- TODO move this to wshs
-- TODO try use haskell.nix
-- TODO only one config.hs file, move other stuff to examples

brewBundle :: Property UnixLike
brewBundle = check (pure True :: IO Bool) $ theWork
  where
    theWork = property "brew bundle"$ do
      liftIO $ do
        home <- myHomeDir
        let brewfile = home </> "Brewfile"
        void $ boolSystem "brew" (map Param ["update"])
        let bundleArgs = ["bundle", "--file", brewfile]
        cmdResult <$> boolSystem "brew" (map Param bundleArgs)

glamdring :: Host
glamdring = host "glamdring" $ props & brewBundle