#!/usr/bin/env stack
-- stack --resolver lts-16.10 script --package turtle --package text --package string-interpolate --package directory
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.String.Interpolate (i)
import           Data.Text
import qualified Data.Text.IO            as TIO
import           Prelude                 hiding (FilePath)
import           System.Directory
import           Turtle

data Mode
  = AddFile FilePath
  | ListFiles
  deriving (Eq, Show)

parser :: Parser Mode
parser =
  AddFile <$> subcommand "add" "add file to bitwarden" (argPath "thepath" "path to the file you want to add")
  <|>
  subcommand "list" "list files in bitwarden" (pure ()) *> (pure ListFiles)

main :: IO ()
main = sh $ do
  mode <- options "Greeting script" parser
  case mode of
    AddFile path -> addFileToBitwarden path
    ListFiles    -> listFiles

addFileToBitwarden :: FilePath -> Shell ()
addFileToBitwarden file = do
  contents <- liftIO $ readTextFile file
  file' <- (liftIO $ makeAbsolute (encodeString file)) >>= (normalizeFileName . decodeString)
  dirId <- ensureBwwFilesDir
  -- soi am having issues with quoting the contents of the file I am trying to add to the store.
  -- i believe
  -- TODO fix it, i dont know how, but fix it
  -- use --arg etc, and just load value directly from the file into jq using like --arg text "$(cat #{absoluteFileName})"
  view $ shells [i|
    echo '{"type":2,"name":"","notes":"","secureNote":{"type":0}}' | jq '.name="file:#{encodeString file'}" | .notes = "#{contents}" | .folderId = "#{dirId}"' | bw encode | bw create item
  |] mempty

ensureBwwFilesDir :: Shell Text
ensureBwwFilesDir = do
  id <- inshell [i|
    bw list folders --search bww_files | jq '. | map(select(.name == "bww_files")) | first | .id | tostring' | sed 's/"//g'
  |] mempty <&> lineToText
  if id == "null" then do
    inshell [i|
      jq '.name = "bww_files"' | bw encode | bw create folder | jq '.id' | sed 's/"//g'
    |] (pure "{}") <&> lineToText
  else
    return id

listFiles :: Shell ()
listFiles = do
  folderId <- ensureBwwFilesDir
  view $ shells [i|
    bw list items --folderid #{folderId} | jq '.[].name'
  |] mempty

normalizeFileName :: FilePath -> Shell FilePath
normalizeFileName fileName = do
  h <- home
  let pat = begins (fromString (encodeString h) *> pure "~")

  let matched = match pat (pack $ encodeString fileName)
  case matched of
    (x:_) -> pure $ fromText x
    _     -> pure fileName
