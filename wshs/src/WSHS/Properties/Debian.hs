-- |

module WSHS.Properties.Debian where

import WSHS.Properties.Core
import WSHS.Util
import Data.Text (Text, isPrefixOf, isInfixOf)
import Turtle

-- | Adds an 'AptRepository' using apt-add-source.
addRepository :: Text -> Property
addRepository repo =
  let checker :: IO PropertyCheckResults
      checker = do
        rawSourcesLines <- shellToList $ inproc "/bin/sh" ["-c", "cat /etc/apt/sources.list /etc/apt/sources.list.d/*"] mempty
        let textLines = rawSourcesLines <&> lineToText
        let removeComments = filter (not . isPrefixOf "#")
        let removeBlanks = filter (/= "")
        let activeSources = removeComments $ removeBlanks textLines
        let repoExists = isInfixOf repo <$> activeSources
        return $ isSatisfied $ or $ repoExists
      satisfier :: IO ()
      satisfier = do
        void $ proc "add-apt-source" [repo] mempty
  in
    Property {..}
