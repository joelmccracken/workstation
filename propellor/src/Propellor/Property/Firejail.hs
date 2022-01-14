-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Firejail (
	installed,
	jailed,
) where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File

-- | Ensures that Firejail is installed
installed :: Property DebianLike
installed = Apt.installed ["firejail"]

-- | For each program name passed, create symlinks in /usr/local/bin that
-- will launch that program in a Firejail sandbox.
--
-- The profile for the sandbox will be the same as if the user had run
-- @firejail@ directly without passing @--profile@ (see "SECURITY PROFILES" in
-- firejail(1)).
--
-- See "DESKTOP INTEGRATION" in firejail(1).
jailed :: [String] -> Property DebianLike
jailed ps = mconcat (map jailed' ps)
	`requires` installed
	`describe` unwords ("firejail jailed":ps)

jailed' :: String -> RevertableProperty UnixLike UnixLike
jailed' p = ("/usr/local/bin" </> p)
	`File.isSymlinkedTo` File.LinkTarget "/usr/bin/firejail"
