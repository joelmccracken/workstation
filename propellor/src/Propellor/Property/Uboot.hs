module Propellor.Property.Uboot where

import Propellor.Base
import Propellor.Types.Info
import Propellor.Types.Bootloader
import Propellor.Types.Container
import Propellor.Property.Mount
import qualified Propellor.Property.Apt as Apt

-- | Name of a board.
type BoardName = String

-- | Installs u-boot for Allwinner/sunxi platforms.
--
-- This includes writing it to the boot sector.
sunxi :: BoardName -> Property (HasInfo + DebianLike)
sunxi boardname = setInfoProperty (check (not <$> hasContainerCapability FilesystemContained) go) info
	`requires` Apt.installed ["u-boot", "u-boot-sunxi"]
  where
	go :: Property Linux
	go = property' "u-boot installed" $ \w -> do
		v <- liftIO $ getMountContaining "/boot"
		case v of
			Nothing -> error "unable to determine boot device"
			Just dev -> ensureProperty w (dd dev "/")
	dd :: FilePath -> FilePath -> Property Linux
	dd dev prefix = tightenTargets $ cmdProperty "dd"
		[ "conv=fsync,notrunc"
		, "if=" ++ prefix ++ "/usr/lib/u-boot/"
			++ boardname ++ "/u-boot-sunxi-with-spl.bin"
		, "of=" ++ dev
		, "bs=1024"
		, "seek=8"
		]
		`assume` NoChange
	info = toInfo [UbootInstalled dd]
