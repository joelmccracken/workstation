#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.turtle p.text p.string-interpolate p.directory p.bytestring p.aeson])"
#! nix-shell -i "runhaskell --ghc-arg='-Wall'"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

import           Data.Aeson
import           Data.ByteString.Lazy    (fromStrict)
import           Data.String.Interpolate (i)
import           Data.Text
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           Prelude                 hiding (FilePath)
import           System.Directory
import           Turtle

data Mode
  = AddFile FilePath
  | ListFiles
  | SyncFiles
  deriving (Eq, Show)

{-
notes
will need to use like e.g.
BW_SESSION="THE KEY" bin/bww.hs force-sync
get this key via `bw unlock`

need to run `bw sync` to pick up on changes that have happend on the server.
-}


parser :: Parser Mode
parser =
  AddFile <$> subcommand "add" "add file to bitwarden" (argPath "thepath" "path to the file you want to add")
  <|>
  subcommand "list" "list files in bitwarden" (pure ()) *> (pure ListFiles)
  <|>
  subcommand "force-sync" "download files, replacing any local files with the remote versions" (pure ()) *> (pure SyncFiles)

main :: IO ()
main = sh $ do
  mode <- options "Bitwarden wrapper" parser
  case mode of
    AddFile path -> addFileToBitwarden path
    ListFiles    -> listFiles
    SyncFiles    -> syncFiles

addFileToBitwarden :: FilePath -> Shell ()
addFileToBitwarden file = do
  file' <- (liftIO $ makeAbsolute (encodeString file)) >>= (normalizeFileName . decodeString)
  dirId <- ensureBwwFilesDir
  view $ shells [i|
    echo '{"type":2,"name":"","notes":"","secureNote":{"type":0}}' | jq --arg contents "$(cat #{encodeString file})" '.name="file:#{encodeString file'}" | .notes = $contents | .folderId = "#{dirId}"' | bw encode | bw create item
  |] mempty

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
normalizeFileName fn = do
  h <- home
  let pat = begins (fromString (encodeString h) *> pure "~")

  let matched = match pat (pack $ encodeString fn)
  case matched of
    (x':_) -> pure $ fromText x'
    _      -> pure fn

tshow :: Show a => a -> Text
tshow = pack . show

syncFiles :: Shell ()
syncFiles = do
  folderId <- ensureBwwFilesDir
  let cmd = [i| bw list items --folderid #{folderId} |]
  filesJson <- lineToText <$> inshell cmd mempty
  let tmp = eitherDecode ( fromStrict . encodeUtf8 $ filesJson)
  liftIO $ TIO.putStrLn $ tshow ( tmp :: Either String [File])
  -- liftIO $ TIO.putStrLn fileName
  -- let cmd2 = [i| bw get item '#{fileName}' | jq -r '.notes' |]
  -- fileContent <- lineToText <$> inshell cmd2 mempty
  -- liftIO $ TIO.putStrLn fileContent

data File = File { fileName :: Text, fileId :: Text }
  deriving (Eq, Show)

instance FromJSON File where
  parseJSON (Object file) =
    File <$> file .: "name" <*> file .: "id"
  parseJSON _ = fail "expect a json object, got another type"
