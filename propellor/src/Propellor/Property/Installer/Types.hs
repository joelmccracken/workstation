module Propellor.Property.Installer.Types where

-- | The disk device to install to.
newtype TargetDiskDevice = TargetDiskDevice FilePath
	deriving (Read, Show)

data DiskEraseConfirmed = DiskEraseConfirmed
	deriving (Read, Show)

-- | Class of user input that an installer might prompt for.
class UserInput i where
	-- | Get the disk device the user selected to install to.
	targetDiskDevice :: i -> Maybe TargetDiskDevice
	-- | Check if the user has confirmed they want to erase the target
	-- disk device.
	diskEraseConfirmed :: i -> Maybe DiskEraseConfirmed
