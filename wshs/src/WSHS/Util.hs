-- |

module WSHS.Util (filePathToText, shellToList) where

import Turtle
import qualified Data.Text as T

filePathToText :: Turtle.FilePath -> Text
filePathToText = T.pack . encodeString

-- | run a shell to completion in IO, collecting output in a list
shellToList :: Shell a -> IO [a]
shellToList theShell = fold theShell (Fold (flip (:)) [] id)
