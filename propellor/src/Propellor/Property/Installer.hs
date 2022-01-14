-- | Installer disk image generation
--
-- These modules contain properties that can be used to create a disk
-- image, suitable for booting from removable media, that can perform an
-- interactive or non-interactive installation of a Host's internal disk.
--
-- The disk image is created using propellor. When booted, it runs
-- propellor to install to the desired disk.
--
-- There is no user interface included here. For an example of using
-- this to build a full, interactive installer, see
-- <https://git.joeyh.name/index.cgi/secret-project.git/>

module Propellor.Property.Installer (
	module Propellor.Property.Installer.Types,
	module Propellor.Property.Installer.Target
) where

import Propellor.Property.Installer.Types
import Propellor.Property.Installer.Target
