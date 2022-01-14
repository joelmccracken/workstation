module Propellor.Property.Sudo where

import Data.List

import Propellor.Base
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User

-- | Allows a user to run any command with sudo.
-- If the user has a password, sudo is configured to require it.
-- If not, NOPASSWORD is enabled for the user.
--
-- Writes to the file /etc/sudoers.d/000users rather than the main sudoers
-- file. This file should come before other include files that may eg,
-- allow running more specific commands without a password, since sudo
-- uses the last matching configuration line.
--
-- If the main sudoers file contains a conflicting line for
-- the user for ALL commands, the line will be removed.
--
-- Also ensures that the main sudoers file includes /etc/sudoers.d/
enabledFor :: User -> RevertableProperty DebianLike DebianLike
enabledFor user@(User u) = setup `requires` Apt.installed ["sudo"] <!> cleanup
  where
	setup :: Property UnixLike
	setup = property' desc $ \w -> do
		locked <- liftIO $ isLockedPassword user
		ensureProperty w $ combineProperties desc $ props
			& includessudoersd
			& fileProperty desc
				(modify locked . filter (wanted locked))
				dfile
			& removeconflicting sudoers
	  where
		desc = u ++ " is sudoer"
	
	cleanup :: Property DebianLike
	cleanup = tightenTargets $ combineProperties desc $ props
		& removeconflicting sudoers
		& removeconflicting dfile
	  where
		desc = u ++ " is not sudoer"
	
	removeconflicting = fileProperty "remove conflicting" (filter notuserline)
	
	-- Not reverted because this line is included by default.
	includessudoersd = fileProperty (sudoers ++ " includes " ++ sudoersd) addl sudoers
	  where
		addl content = content ++ 
			if l `notElem` content && oldl `notElem` content
				then [l]
				else []
		l = "@includedir /etc/sudoers.d"
		oldl = "#includedir /etc/sudoers.d"

	sudoers = "/etc/sudoers"
	sudoersd = "/etc/sudoers.d"
	dfile = "/etc/sudoers.d/000users"
	sudobaseline = u ++ " ALL=(ALL:ALL)"
	notuserline l = not (sudobaseline `isPrefixOf` l)
	sudoline True = sudobaseline ++ " NOPASSWD:ALL"
	sudoline False = sudobaseline ++ " ALL"
	wanted locked l
		| notuserline l = True
		| "NOPASSWD" `isInfixOf` l = locked
		| otherwise = True
	modify locked ls
		| sudoline locked `elem` ls = ls
		| otherwise = ls ++ [sudoline locked]

-- | Sets up a file in /etc/sudoers.d/, which /etc/sudoers includes,
-- with the specified content.
--
-- The FilePath can be relative to that directory.
sudoersDFile :: FilePath -> [Line] -> RevertableProperty DebianLike Linux
sudoersDFile dfile content = setup `requires` Apt.installed ["sudo"] <!> cleanup
  where
	f = "/etc/sudoers.d" </> dfile
	-- sudoers.d files should not be world readable
	setup = hasContentProtected f content
	cleanup = tightenTargets $ notPresent f
