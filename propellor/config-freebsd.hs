-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.
--
-- This shows how to set up a FreeBSD host (and a Linux host too).

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network
import qualified Propellor.Property.Cron as Cron
import Propellor.Property.Scheduled
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.FreeBSD.Pkg as Pkg
import qualified Propellor.Property.ZFS as ZFS
import qualified Propellor.Property.FreeBSD.Poudriere as Poudriere

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[ freebsdbox
	, linuxbox
	]

-- An example freebsd host.
freebsdbox :: Host
freebsdbox = host "freebsdbox.example.com" $ props
	& osFreeBSD (FBSDProduction FBSD102) X86_64
	& Pkg.update
	& Pkg.upgrade
	& Poudriere.poudriere poudriereZFS
	& Poudriere.jail (Poudriere.Jail "formail" (fromString "10.2-RELEASE") (fromArchitecture X86_64))

poudriereZFS :: Poudriere.Poudriere
poudriereZFS = Poudriere.defaultConfig
	{ Poudriere._zfs = Just $ Poudriere.PoudriereZFS
		(ZFS.ZFS (fromString "zroot") (fromString "poudriere"))
		(ZFS.fromList [ZFS.Mountpoint (fromString "/poudriere"), ZFS.ACLInherit ZFS.AIPassthrough])
	}

-- An example linux host.
linuxbox :: Host
linuxbox = host "linuxbox.example.com" $ props
	& osDebian' KFreeBSD Unstable X86_64
	& Apt.stdSourcesList
	& Apt.unattendedUpgrades
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh"]
	& User.hasSomePassword (User "root")
	& Network.ipv6to4
	& File.dirExists "/var/www"
	& Docker.docked webserverContainer
	& Docker.garbageCollected `period` Daily
	& Cron.runPropellor (Cron.Times "30 * * * *")

-- A generic webserver in a Docker container.
webserverContainer :: Docker.Container
webserverContainer = Docker.container "webserver" (Docker.latestImage "debian") $ props
	& osDebian' KFreeBSD (Stable "buster") X86_64
	& Apt.stdSourcesList
	& Docker.publish "80:80"
	& Docker.volume "/var/www:/var/www"
	& Apt.serviceInstalledRunning "apache2"
