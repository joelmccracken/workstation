module Propellor.Property.SiteSpecific.GitHome where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User

-- | Clones Joey Hess's git home directory, and runs its fixups script.
installedFor :: User -> Property DebianLike
installedFor user@(User u) = check (not <$> hasGitDir user) $ 
	go `requires` Apt.installed ["git"]
  where
	go :: Property DebianLike
	go = property' ("githome " ++ u) $ \w -> do
		home <- liftIO (homedir user)
		let tmpdir = home </> "githome"
		ensureProperty w $ combineProperties "githome setup" $ toProps
			[ userScriptProperty user ["git clone " ++ url ++ " " ++ tmpdir]
				`assume` MadeChange
			, property "moveout" $ makeChange $ void $
				moveout tmpdir home
			, property "rmdir" $ makeChange $ void $
				catchMaybeIO $ removeDirectory tmpdir
			, userScriptProperty user ["rm -rf .aptitude/ .bashrc .profile"]
				`assume` MadeChange
			-- Set HOSTNAME so that this sees the right
			-- hostname when run in a chroot with a different
			-- hostname than the current one.
			, userScriptProperty user ["HOSTNAME=$(cat /etc/hostname) bin/mr checkout"]
				`assume` MadeChange
			, userScriptProperty user ["bin/fixups"]
				`assume` MadeChange
			]
	moveout tmpdir home = do
		fs <- dirContents tmpdir
		forM fs $ \f -> boolSystem "mv" [File f, File home]

url :: String
url = "git://git.kitenet.net/joey/home"

hasGitDir :: User -> IO Bool
hasGitDir user = go =<< homedir user
  where
	go home = doesDirectoryExist (home </> ".git")
