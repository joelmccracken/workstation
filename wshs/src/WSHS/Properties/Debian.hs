-- |

module WSHS.Properties.Debian where

import WSHS.Properties.Core
import WSHS.Util
import Data.Text (Text, isPrefixOf, isInfixOf)
import Turtle
import qualified Turtle.Bytes as TBytes

import WSHS.Wrappers.Debian.Snap as Snap
import Data.ByteString
import Text.Trifecta (parseByteString, Result(Success, Failure), foldResult)
import qualified Data.List as List
import Data.Maybe (isJust)

-- | Adds an 'AptRepository' using apt-add-source.
addSnap :: Text -> Property
addSnap snapName =
  let checker :: IO PropertyCheckResults
      checker = do
        outputbs <- mconcat <$> (shellToList $ TBytes.inproc "snap" ["list"] mempty)
        let parseResult = parseByteString Snap.snapListCommandOutputParser mempty outputbs
        let snaps = foldResult (error . show) id parseResult
        let snapIsInstalled = isJust $ List.find ((snapName ==) . name) snaps
        return $ isSatisfied snapIsInstalled
      satisfier :: IO ()
      satisfier = do
        void $ proc "snap" ["install", snapName, "--classic"] mempty
  in
    Property {..}
