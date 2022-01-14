module Propellor.Property.Qemu where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt

-- | Installs qemu user mode emulation binaries, built statically,
-- which allow foreign binaries to run directly.
foreignBinariesEmulated :: RevertableProperty Linux Linux
foreignBinariesEmulated = (setup <!> cleanup)
	`describe` "foreign binary emulation"
  where
	setup = Apt.installed p `pickOS` unsupportedOS
	cleanup = Apt.removed p `pickOS` unsupportedOS
	p = ["qemu-user-static"]

-- | Check if the given System supports an Architecture.
--
-- For example, on Debian, X86_64 supports X86_32, and vice-versa.
supportsArch :: System -> Architecture -> Bool
supportsArch (System os a) b
	| a == b = True
	| otherwise = case os of
		Debian _ _ -> debianlike
		Buntish _ -> debianlike
		-- don't know about other OS's
		_ -> False
  where
	debianlike =
		let l = 
			[ (X86_64, X86_32)
			, (ARMHF, ARMEL)
			, (PPC, PPC64)
			, (SPARC, SPARC64)
			, (S390, S390X)
			]
		in elem (a, b) l || elem (b, a) l
