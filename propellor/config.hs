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

main :: IO ()
main = mainProperties myHost

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

myHost :: Host
myHost = host "local" $ props & brewBundle
