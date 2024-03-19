-- |

module WSHS.Properties.MacOS (brewBundled) where

import RIO

import WSHS.Properties.Core
import Turtle
import WSHS.Util

brewBundled :: Property
brewBundled =
  let
    name = "brew bundle is run and up to date"
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
      -- note: note running brew update here as
      -- it almost certainly will find thigns that are out of date,
      -- which basically makes this useless.
      -- probably should have some kind of idea of a parameter/flag for this setting?
      -- so would run something like `ws check --updates`? I dunno
      -- void $ proc "brew" ["update"] mempty
      exitCode <- proc "brew" (["bundle", "check"] ++ fileCommandArgs) mempty
      pure $ isSatisfied (exitCode == ExitSuccess)
  in
    Property {..}
