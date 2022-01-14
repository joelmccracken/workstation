{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Propellor.Types.Bootloader where

import Propellor.Types
import Propellor.Types.Info

-- | Boot loader installed on a host.
data BootloaderInstalled
	= GrubInstalled GrubTarget
	| FlashKernelInstalled
	| UbootInstalled (FilePath -> FilePath -> Property Linux)
	| NoBootloader
	deriving (Typeable)

-- | Platforms that grub can boot.
data GrubTarget = PC | EFI64 | EFI32 | Coreboot | Xen

instance Show BootloaderInstalled where
	show (GrubInstalled _) = "GrubInstalled"
	show FlashKernelInstalled = "FlashKernelInstalled"
	show (UbootInstalled _) = "UbootInstalled"
	show NoBootloader = "NoBootloader"

instance IsInfo [BootloaderInstalled] where
	propagateInfo _ = PropagateInfo False
