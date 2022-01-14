module Propellor.Property.Laptop where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Systemd as Systemd

-- | Makes powertop auto-tune the system for optimal power consumption on
-- boot.
powertopAutoTuneOnBoot :: RevertableProperty DebianLike DebianLike
powertopAutoTuneOnBoot = setup <!> undo
	`describe` "powertop auto-tune on boot"
  where
	setup = Systemd.enabled "powertop"
		`requires` Apt.installed ["powertop"]
		`requires` File.hasContent servicefile
			[ "[Unit]"
			, "Description=Powertop tunings"
			, "[Service]"
			, "ExecStart=/usr/sbin/powertop --auto-tune"
			, "RemainAfterExit=true"
			, "[Install]"
			, "WantedBy=multi-user.target"
			]
	undo = tightenTargets $ File.notPresent servicefile
		`requires` check (doesFileExist servicefile)
			(Systemd.disabled "powertop")
	servicefile = "/etc/systemd/system/powertop.service"

-- | Enables weekly TRIM for SSDs, using systemd's fstrim.timer,
trimSSD :: Property Linux
trimSSD = Systemd.enabled "fstrim.timer"
