module Propellor.Property.HostingProvider.Linode where

import Propellor.Base
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.File as File

-- | Configures grub to use the serial console as set up by Linode.
-- Useful when running a distribution supplied kernel.
-- <https://www.linode.com/docs/tools-reference/custom-kernels-distros/run-a-distribution-supplied-kernel-with-kvm>
serialGrub :: Property (HasInfo + DebianLike)
serialGrub = "/etc/default/grub" `File.containsLines`
	[ "GRUB_CMDLINE_LINUX=\"console=ttyS0,19200n8\""
	, "GRUB_DISABLE_LINUX_UUID=true"
	, "GRUB_SERIAL_COMMAND=\"serial --speed=19200 --unit=0 --word=8 --parity=no --stop=1\""
	, "GRUB_TERMINAL=serial"
	]
	`onChange` Grub.mkConfig
	`requires` Grub.installed Grub.PC
	`describe` "GRUB configured for Linode serial console"

-- | Linode's pv-grub-x86_64 (only used for its older XEN instances)
-- does not support booting recent Debian kernels compressed
-- with xz. This sets up pv-grub chaining to enable it.
chainPVGrub :: Grub.TimeoutSecs -> Property (HasInfo + DebianLike)
chainPVGrub = Grub.chainPVGrub "hd0" "xen/xvda"

-- | Linode disables mlocate's cron job's execute permissions,
-- presumably to avoid disk IO. This ensures it's executable,
-- if it's installed. It does the same for its replacement plocate,
-- in cae Linode starts messing with that.
locateEnabled :: Property DebianLike
locateEnabled = tightenTargets $
	propertyList "locate enabled" $ props
		& go "/etc/cron.daily/mlocate"
		& go "/etc/cron.daily/plocate"
  where
	go f = check (doesFileExist f)
			(f `File.mode` combineModes (readModes ++ executeModes))
