module Propellor.PrivData.Paths where

import Utility.Exception
import System.FilePath
import Control.Applicative
import Prelude

privDataDir :: FilePath
privDataDir = "privdata"

privDataFile :: IO FilePath
privDataFile = allowRelocate $ privDataDir </> "privdata.gpg"

privDataKeyring :: IO FilePath
privDataKeyring = allowRelocate $ privDataDir </> "keyring.gpg"

privDataLocal :: FilePath
privDataLocal = privDataDir </> "local"

privDataRelay :: String -> FilePath
privDataRelay host = privDataDir </> "relay" </> host

-- Allow relocating files in privdata, by checking for a file
-- privdata/relocate, which contains the path to a subdirectory that
-- contains the files.
allowRelocate :: FilePath -> IO FilePath
allowRelocate f = reloc . lines
	<$> catchDefaultIO "" (readFile (privDataDir </> "relocate"))
  where
	reloc (p:_) | not (null p) = privDataDir </> p </> takeFileName f
	reloc _ = f
