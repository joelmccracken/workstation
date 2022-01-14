module Propellor.Property.HostingProvider.DigitalOcean (
	distroKernel
) where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Reboot as Reboot

-- | Digital Ocean does not provide any way to boot
-- the kernel provided by the distribution, except using kexec.
-- Without this, some old, and perhaps insecure kernel will be used.
--
-- This property causes the distro kernel to be loaded on reboot, using kexec.
--
-- When the power is cycled, the non-distro kernel still boots up.
-- So, this property also checks if the running kernel is present in /boot,
-- and if not, reboots immediately into a distro kernel.
distroKernel :: Property DebianLike
distroKernel = propertyList "digital ocean distro kernel hack" $ props
	& Apt.installed ["grub-pc", "kexec-tools", "file"]
	& "/etc/default/kexec" `File.containsLines`
		[ "LOAD_KEXEC=true"
		, "USE_GRUB_CONFIG=true"
		] `describe` "kexec configured"
	& Reboot.toDistroKernel
