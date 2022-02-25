{-# LANGUAGE OverloadedStrings #-}

-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import Propellor.Engine
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Cmd as Cmd
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Apt.PPA as PPA
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
            "ci-macos" -> ciMacos
            "ci-ubuntu" -> ubuntu
            _ -> error $ T.unpack ("unknown host name " <>  machineName options)

  mainProperties $ theHost

-- TODO move this to wshs
-- TODO try use haskell.nix
-- TODO only one config.hs file, move other stuff to examples

-- this is the version that doom recommends using
emacsPPA :: PPA.AptRepository
emacsPPA = PPA.AptRepositoryPPA "ppa:kelleyk/emacs"

ubuntu :: Host
ubuntu = host "ci-ubuntu" $ props
  & PPA.addRepository emacsPPA
  & Apt.update
  & Apt.installed ["emacs27"]

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

ciMacos :: Host
ciMacos = host "ci-macos" $ props & brewBundle
