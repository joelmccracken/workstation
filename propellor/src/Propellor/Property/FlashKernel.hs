-- | Make ARM systems bootable using Debian's flash-kernel package.

module Propellor.Property.FlashKernel where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.Mount
import Propellor.Types.Bootloader
import Propellor.Types.Info

-- | A machine name, such as "Cubietech Cubietruck" or "Olimex A10-OLinuXino-LIME"
--
-- flash-kernel supports many different machines,
-- see its file /usr/share/flash-kernel/db/all.db for a list.
type Machine = String

-- | Uses flash-kernel to make a machine bootable.
--
-- Before using this, an appropriate kernel needs to already be installed, 
-- and on many machines, u-boot needs to be installed too.
installed :: Machine -> Property (HasInfo + DebianLike)
installed machine = setInfoProperty go (toInfo [FlashKernelInstalled])
  where
	go = Apt.installed ["flash-kernel"]
		`requires` configured
		`onChange` flashKernel
	configured = ("/etc/flash-kernel/machine" `File.hasContent` [machine])
		`requires` File.dirExists "/etc/flash-kernel"

-- | Runs flash-kernel with whatever machine `installed` configured.
flashKernel :: Property DebianLike
flashKernel = tightenTargets $
	cmdProperty "flash-kernel" [] `assume` MadeChange

-- | Runs flash-kernel in the system mounted at a particular directory.
flashKernelMounted :: FilePath -> Property Linux
flashKernelMounted mnt = combineProperties desc $ props
	-- remove mounts that are done below to make sure the right thing
	-- gets mounted
	& cleanupmounts
	& bindMount "/dev" (inmnt "/dev")
	& mounted "proc" "proc" (inmnt "/proc") mempty
	& mounted "sysfs" "sys" (inmnt "/sys") mempty
	-- update the initramfs so it gets the uuid of the root partition
	& inchroot "update-initramfs" ["-u"]
		`assume` MadeChange
	& inchroot "flash-kernel" []
		`assume` MadeChange
	& cleanupmounts
  where
	desc = "flash-kernel run"

	-- cannot use </> since the filepath is absolute
	inmnt f = mnt ++ f

	inchroot cmd ps = cmdProperty "chroot" ([mnt, cmd] ++ ps)

	cleanupmounts :: Property Linux
	cleanupmounts = property desc $ liftIO $ do
		cleanup "/sys"
		cleanup "/proc"
		cleanup "/dev"
		return NoChange
	  where
		cleanup m =
			let mp = inmnt m
			in whenM (isMounted mp) $
				umountLazy mp
