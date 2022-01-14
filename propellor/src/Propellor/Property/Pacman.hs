-- | Maintainer: Zihao Wang <dev@wzhd.org>
--
-- Support for the Pacman package manager <https://www.archlinux.org/pacman/>

module Propellor.Property.Pacman where

import Propellor.Base

runPacman :: [String] -> UncheckedProperty ArchLinux
runPacman ps = tightenTargets $ cmdProperty "pacman" ps

-- | Have pacman update its lists of packages, but without upgrading anything.
update :: Property ArchLinux
update =  combineProperties ("pacman update") $ props
	& runPacman ["-Sy", "--noconfirm"]
		`assume` MadeChange

upgrade :: Property ArchLinux
upgrade = combineProperties ("pacman upgrade") $ props
	& runPacman ["-Syu", "--noconfirm"]
		`assume` MadeChange

type Package = String

installed :: [Package] -> Property ArchLinux
installed = installed' ["--noconfirm"]

installed' :: [String] -> [Package] -> Property ArchLinux
installed' params ps = check (not <$> isInstalled' ps) go
	`describe` unwords ("pacman installed":ps)
  where
	go = runPacman (params ++ ["-S"] ++ ps)

removed :: [Package] -> Property ArchLinux
removed ps = check (any (== IsInstalled) <$> getInstallStatus ps)
	(runPacman (["-R", "--noconfirm"] ++ ps))
	`describe` unwords ("pacman removed":ps)

isInstalled :: Package -> IO Bool
isInstalled p = isInstalled' [p]

isInstalled' :: [Package] -> IO Bool
isInstalled' ps = all (== IsInstalled) <$> getInstallStatus ps

data InstallStatus = IsInstalled | NotInstalled
	deriving (Show, Eq)

{- Returns the InstallStatus of packages that are installed
 - or known and not installed. If a package is not known at all to apt
 - or dpkg, it is not included in the list. -}
getInstallStatus :: [Package] -> IO [InstallStatus]
getInstallStatus ps = mapMaybe id <$> mapM status ps
  where
	status :: Package -> IO (Maybe InstallStatus)
	status p = do
	  ifM (succeeds "pacman" ["-Q", p])
	    (return (Just IsInstalled),
	      ifM (succeeds "pacman" ["-Sp", p])
	        (return (Just NotInstalled),
	         return Nothing))

succeeds :: String -> [String] -> IO Bool
succeeds cmd args = (quietProcess >> return True)
	`catchIO` (\_ -> return False)
  where
	quietProcess :: IO ()
	quietProcess = withQuietOutput createProcessSuccess p
	p = (proc cmd args)
