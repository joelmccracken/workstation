-- |

module WSHS.Properties.MacOS (brewBundled) where

import RIO

import WSHS.Properties.Core
import Turtle
import WSHS.Util

brewBundled :: Property
brewBundled =
  let
    getBrewfile :: IO Turtle.FilePath
    getBrewfile = do
      h <- home
      pure $ (h </> "Brewfile")

    satisfier :: IO ()
    satisfier = do
      brewfile <- filePathToText <$> getBrewfile
      let fileCommandArgs = ["--file", brewfile]
      void $ proc "brew" ["update"] mempty
      void $ proc "brew" (["bundle"] ++ fileCommandArgs) mempty

    checker :: IO PropertyCheckResults
    checker = do
      brewfile <- filePathToText <$> getBrewfile
      let fileCommandArgs = ["--file", brewfile]
      void $ proc "brew" ["update"] mempty
      exitCode <- proc "brew" (["bundle", "check"] ++ fileCommandArgs) mempty
      pure $ isSatisfied (exitCode == ExitSuccess)
  in
    Property {..}
