module Propellor.Property.XFCE where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User

installed :: Property DebianLike
installed = Apt.installed ["task-xfce-desktop"]
	`describe` "XFCE desktop installed"

-- | Minimal install of XFCE, with a terminal emulator and panel,
-- and X and network-manager, but not any of the extra apps.
installedMin :: Property DebianLike
installedMin = Apt.installedMin ["xfce4", "xfce4-terminal", "task-desktop"]
	`describe` "minimal XFCE desktop installed"

-- | Installs network-manager-gnome, which is the way to get
-- network-manager to manage networking in XFCE too.
networkManager :: Property DebianLike
networkManager = Apt.installedMin ["network-manager-gnome"]

-- | Normally at first login, XFCE asks what kind of panel the user wants.
-- This enables the default configuration noninteractively.
defaultPanelFor :: User -> File.Overwrite -> Property DebianLike
defaultPanelFor u@(User username) overwrite = property' desc $ \w -> do
	home <- liftIO $ User.homedir u
	ensureProperty w (go home)
  where
	desc = "default XFCE panel for " ++ username
	basecf = ".config" </> "xfce4" </> "xfconf"
		</> "xfce-perchannel-xml" </> "xfce4-panel.xml"
	-- This location is probably Debian-specific.
	defcf = "/etc/xdg/xfce4/panel/default.xml"
	go :: FilePath -> Property DebianLike
	go home = tightenTargets $ 
		File.checkOverwrite overwrite (home </> basecf) $ \cf ->
			cf `File.isCopyOf` defcf
				`before` File.applyPath home basecf
					(\f -> File.ownerGroup f u (userGroup u))
				`requires` Apt.installed ["xfce4-panel"]
