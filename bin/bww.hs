#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.turtle p.text p.string-interpolate p.directory])"
#! nix-shell -i "runhaskell --ghc-arg='-Wall'"

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
  | PullFiles
  deriving (Eq, Show)

parser :: Parser Mode
parser =
  AddFile <$> subcommand "add" "add file to bitwarden" (argPath "thepath" "path to the file you want to add")
  <|>
  subcommand "list" "list files in bitwarden" (pure ()) *> (pure ListFiles)
  <|>
  subcommand "pull" "pull all files down to the local machine" (switch "force" 'f' "replace local file if it already exists and has different contents") *> pure PullFiles

main :: IO ()
main = sh $ do
  mode <- options "Greeting script" parser
  case mode of
    AddFile path -> addFileToBitwarden path
    ListFiles    -> listFiles
    PullFiles    -> pullFiles

addFileToBitwarden :: FilePath -> Shell ()
addFileToBitwarden file = do
  file' <- (liftIO $ makeAbsolute (encodeString file)) >>= (normalizeFileName . decodeString)
  dirId <- ensureBwwFilesDir
  view $ shells [i|
    echo '{"type":2,"name":"","notes":"","secureNote":{"type":0}}' | jq --arg contents "$(cat #{encodeString file})" '.name="file:#{encodeString file'}" | .notes = $contents | .folderId = "#{dirId}"' | bw encode | bw create item
  |] mempty

-- TODO enable warnings for this script

ensureBwwFilesDir :: Shell Text
ensureBwwFilesDir = do
  id' <- inshell [i|
    bw list folders --search bww_files | jq '. | map(select(.name == "bww_files")) | first | .id | tostring' | sed 's/"//g'
  |] mempty <&> lineToText
  if id' == "null" then do
    inshell [i|
      jq '.name = "bww_files"' | bw encode | bw create folder | jq '.id' | sed 's/"//g'
    |] (pure "{}") <&> lineToText
  else
    return id'

listFiles :: Shell ()
listFiles = do
  file <- getFilesList
  liftIO $ TIO.putStrLn file

getFilesList :: Shell Text
getFilesList = do
  folderId <- ensureBwwFilesDir
  lineToText <$> inshell [i| bw list items --folderid #{folderId} | jq '.[].name' | sed 's/^"//g' | sed 's/"$//g'  |] mempty

normalizeFileName :: FilePath -> Shell FilePath
normalizeFileName fileName = do
  h <- home
  let pat = begins (fromString (encodeString h) *> pure "~")

  let matched = match pat (pack $ encodeString fileName)
  case matched of
    (x':_) -> pure $ fromText x'
    _      -> pure fileName

pullFiles :: Shell ()
pullFiles = error "yea"
