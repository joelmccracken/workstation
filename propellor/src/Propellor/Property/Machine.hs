-- | Machine-specific properties.
--
-- Many embedded computers have their own special configuration needed
-- to use them. Rather than needing to hunt down documentation about the
-- kernel, bootloader, etc for a given machine, if there's a property
-- in here for your machine, you can simply use it.
--
-- Not all machine properties have been tested yet. If one flagged as
-- untested and you find it works, please let us know.
--
-- You will need to configure the `Host` with the right `Architecture`
-- for the machine. These properties do test at runtime that a supported
-- Architecture was selected.
--
-- Sometimes non-free firmware is needed to use a board. If the board won't
-- be functional at all without it, its property will include the non-free
-- firmware, but if the non-free firmware is only needed for non-critical
-- functionality, it won't be included.
-- 
-- Example: Building a disk image for a Marvell SheevaPlug 
--
-- This defines a Host "sheeva" that is a Marvell SheevaPlug.
-- A bootable disk image for "sheeva" is built on another machine
-- "darkstar", which can be eg an Intel laptop running Debian.
--
-- > import Propellor.Property.Machine
-- > import Propellor.Property.DiskImage
-- > 
-- > sheeva :: Host
-- > sheeva = host "sheeva.example.com" $ props
-- > 	& osDebian Unstable ARMEL
-- > 	& marvell_SheevaPlug Marvell_SheevaPlug_SDCard
-- >	& hasPartition
-- >		( partition EXT4
-- > 		`mountedAt` "/"
-- >		`addFreeSpace` MegaBytes 2048
-- >		)
-- >
-- > darkstar :: Host
-- > darkstar = host "darkstar.example.com" $ props
-- >	& imageBuiltFor sheeva
-- >		(RawDiskImage "/srv/sheeva-disk.img")
-- >		(Debootstrapped mempty)

module Propellor.Property.Machine (
	-- * ARM boards
	Marvell_SheevaPlug_BootDevice(..),
	marvell_SheevaPlug,
	cubietech_Cubietruck,
	olimex_A10_OLinuXino_LIME,
	lemaker_Banana_Pi,
	-- * ARM boards (untested)
	cubietech_Cubieboard,
	cubietech_Cubieboard2,
	lemaker_Banana_Pro,
	olimex_A10s_OLinuXino_Micro,
	olimex_A20_OLinuXino_LIME,
	olimex_A20_OLinuXino_LIME2,
	olimex_A20_OLinuXino_Micro,
	olimex_A20_SOM_EVB,
	linkSprite_pcDuino3_Nano,
) where

import Propellor.Base
import Propellor.Types.Core
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.FlashKernel as FlashKernel
import qualified Propellor.Property.Uboot as Uboot
import Propellor.Property.DiskImage.PartSpec

data Marvell_SheevaPlug_BootDevice
	= Marvell_SheevaPlug_SDCard
	| Marvell_SheevaPlug_ESATA

-- | Marvell SheevaPlug
--
-- This includes a small EXT2 formatted /boot partition.
--
-- Note that u-boot may need to be upgraded manually, and will need to be
-- configured to boot from the SD card or eSATA. See
-- https://www.cyrius.com/debian/kirkwood/sheevaplug/install/
marvell_SheevaPlug :: Marvell_SheevaPlug_BootDevice -> Property (HasInfo + DebianLike)
marvell_SheevaPlug bd = fk
	`requires` marvell
	`requires` hasPartition bootpart
  where
	fk = case bd of
		Marvell_SheevaPlug_SDCard ->
			FlashKernel.installed "Marvell SheevaPlug Reference Board"
		Marvell_SheevaPlug_ESATA ->
			FlashKernel.installed "Marvell eSATA SheevaPlug Reference Board"
	-- The boot loader needs an EXT2 boot partition, which comes
	-- first. Add some free space to allow for additional kernel images
	-- and initrds.
	bootpart :: PartSpec PartLocation
	bootpart = partition EXT2
		`mountedAt` "/boot"
		`partLocation` Beginning
		`addFreeSpace` MegaBytes 150

-- | Cubietech Cubietruck
-- 
-- Wifi needs non-free firmware-brcm80211, which is not installed by
-- this property. Also, see https://bugs.debian.org/844056
cubietech_Cubietruck :: Property (HasInfo + DebianLike)
cubietech_Cubietruck = FlashKernel.installed "Cubietech Cubietruck"
	`requires` sunixi "Cubietruck"
	`requires` lpae

-- | Cubietech Cubieboard (untested)
cubietech_Cubieboard :: Property (HasInfo + DebianLike)
cubietech_Cubieboard = FlashKernel.installed "Cubietech Cubieboard"
	`requires` sunixi "Cubieboard"
	`requires` armmp

-- | Cubietech Cubieboard2 (untested)
cubietech_Cubieboard2 :: Property (HasInfo + DebianLike)
cubietech_Cubieboard2 = FlashKernel.installed "Cubietech Cubieboard2"
	`requires` sunixi "Cubieboard2"
	`requires` lpae

-- | LeMaker Banana Pi
lemaker_Banana_Pi :: Property (HasInfo + DebianLike)
lemaker_Banana_Pi = FlashKernel.installed "LeMaker Banana Pi"
	`requires` sunixi "Bananapi"
	`requires` lpae

-- | LeMaker Banana Pro (untested)
lemaker_Banana_Pro :: Property (HasInfo + DebianLike)
lemaker_Banana_Pro = FlashKernel.installed "LeMaker Banana Pro"
	`requires` sunixi "Bananapro"
	`requires` lpae

-- | Olimex A10-OLinuXino-LIME
olimex_A10_OLinuXino_LIME :: Property (HasInfo + DebianLike)
olimex_A10_OLinuXino_LIME = FlashKernel.installed "Olimex A10-OLinuXino-LIME"
	`requires` sunixi "A10-OLinuXino-Lime"
	`requires` armmp

-- | Olimex A10s-Olinuxino Micro (untested)
olimex_A10s_OLinuXino_Micro :: Property (HasInfo + DebianLike)
olimex_A10s_OLinuXino_Micro = FlashKernel.installed "Olimex A10s-Olinuxino Micro"
	`requires` sunixi "A10s-OLinuXino-M"
	`requires` armmp

-- | Olimex A20-OlinuXino-LIME (untested)
olimex_A20_OLinuXino_LIME :: Property (HasInfo + DebianLike)
olimex_A20_OLinuXino_LIME = FlashKernel.installed "Olimex A20-OLinuXino-LIME"
	`requires` sunixi "A20-OLinuXino-Lime"
	`requires` lpae

-- | Olimex A20-OlinuXino-LIME2 (untested)
olimex_A20_OLinuXino_LIME2 :: Property (HasInfo + DebianLike)
olimex_A20_OLinuXino_LIME2 = FlashKernel.installed "Olimex A20-OLinuXino-LIME2"
	`requires` sunixi "A20-OLinuXino-Lime2"
	`requires` lpae

-- | Olimex A20-Olinuxino Micro (untested)
olimex_A20_OLinuXino_Micro :: Property (HasInfo + DebianLike)
olimex_A20_OLinuXino_Micro = FlashKernel.installed "Olimex A20-Olinuxino Micro"
	`requires` sunixi "A20-OLinuXino-MICRO"
	`requires` lpae

-- | Olimex A20-SOM-EVB (untested)
olimex_A20_SOM_EVB :: Property (HasInfo + DebianLike)
olimex_A20_SOM_EVB = FlashKernel.installed "Olimex A20-Olimex-SOM-EVB"
	`requires` sunixi "A20-Olimex-SOM-EVB"
	`requires` lpae

-- | LinkSprite pcDuino Nano (untested)
--
-- Needs non-free firmware, see
-- https://wiki.debian.org/InstallingDebianOn/Allwinner
linkSprite_pcDuino3_Nano :: Property (HasInfo + DebianLike)
linkSprite_pcDuino3_Nano = FlashKernel.installed "LinkSprite pcDuino3 Nano"
	`requires` sunixi "Linksprite_pcDuino3"
	`requires` lpae

sunixi :: Uboot.BoardName -> Property (HasInfo + DebianLike)
sunixi boardname = Uboot.sunxi boardname
	`requires` Apt.installed
		[ "firmware-linux-free"
		, "sunxi-tools"
		]

armmp :: Property DebianLike
armmp = checkArchitecture [ARMHF, ARMEL] $
	Apt.installed ["linux-image-armmp"]

lpae :: Property DebianLike
lpae = checkArchitecture [ARMHF, ARMEL] $ 
	Apt.installed ["linux-image-armmp-lpae"]

marvell :: Property DebianLike
marvell = checkArchitecture [ARMEL] $
	Apt.installed ["linux-image-marvell"]

checkArchitecture :: [Architecture] -> Property DebianLike -> Property DebianLike
checkArchitecture as p = withOS (getDesc p) $ \w o -> case o of
	(Just (System _ arch)) | arch `elem` as -> ensureProperty w p
	_ -> error $ "Machine needs architecture to be one of: " ++ show as
