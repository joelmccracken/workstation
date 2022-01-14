module Propellor.Property.Rsync where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Pacman as Pacman

type Src = FilePath
type Dest = FilePath

class RsyncParam p where
	toRsync :: p -> String

-- | A pattern that matches all files under a directory, but does not
-- match the directory itself.
filesUnder :: FilePath -> Pattern
filesUnder d = Pattern (d ++ "/*")

-- | Ensures that the Dest directory exists and has identical contents as
-- the Src directory.
syncDir :: Src -> Dest -> Property (DebianLike + ArchLinux)
syncDir = syncDirFiltered []

data Filter 
	= Include Pattern
	| Exclude Pattern
	| Protect Pattern

instance RsyncParam Filter where
	toRsync (Include (Pattern p)) = "--include=" ++ p
	toRsync (Exclude (Pattern p)) = "--exclude=" ++ p
	toRsync (Protect (Pattern p)) = "--filter=P " ++ p

-- | A pattern to match against files that rsync is going to transfer.
--
-- See "INCLUDE/EXCLUDE PATTERN RULES" in the rsync(1) man page.
--
-- For example, Pattern "/foo/*" matches all files under the "foo"
-- directory, relative to the 'Src' that rsync is acting on.
newtype Pattern = Pattern String

-- | Like syncDir, but avoids copying anything that the filter list
-- excludes. Anything that's filtered out will be deleted from Dest.
--
-- Rsync checks each name to be transferred against its list of Filter
-- rules, and the first matching one is acted on. If no matching rule
-- is found, the file is processed.
syncDirFiltered :: [Filter] -> Src -> Dest -> Property (DebianLike + ArchLinux)
syncDirFiltered filters src dest = rsync $
	[ "-a"
	-- Add trailing '/' to get rsync to sync the Dest directory,
	-- rather than a subdir inside it, which it will do without a
	-- trailing '/'.
	, addTrailingPathSeparator src
	, addTrailingPathSeparator dest
	, "--delete"
	, "--delete-excluded"
	, "--info=progress2"
	] ++ map toRsync filters

rsync :: [String] -> Property (DebianLike + ArchLinux)
rsync ps = cmdProperty "rsync" ps
	`assume` MadeChange
	`requires` installed

installed :: Property (DebianLike + ArchLinux)
installed = Apt.installed ["rsync"] `pickOS` Pacman.installed ["rsync"]
