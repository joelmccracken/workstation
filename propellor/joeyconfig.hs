-- This is the live config file used by propellor's author.
-- https://propellor.branchable.com/
module Main where

import Propellor
import Propellor.Property.Scheduled
import Propellor.Property.DiskImage
import Propellor.Property.Chroot
import Propellor.Property.Machine
import Propellor.Property.Bootstrap
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Hostname as Hostname
import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Dns as Dns
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Postfix as Postfix
import qualified Propellor.Property.Apache as Apache
import qualified Propellor.Property.LetsEncrypt as LetsEncrypt
import qualified Propellor.Property.Locale as Locale
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.Borg as Borg
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Journald as Journald
import qualified Propellor.Property.Fail2Ban as Fail2Ban
import qualified Propellor.Property.Laptop as Laptop
import qualified Propellor.Property.LightDM as LightDM
import qualified Propellor.Property.HostingProvider.Linode as Linode
import qualified Propellor.Property.SiteSpecific.GitHome as GitHome
import qualified Propellor.Property.SiteSpecific.GitAnnexBuilder as GitAnnexBuilder
import qualified Propellor.Property.SiteSpecific.Branchable as Branchable
import qualified Propellor.Property.SiteSpecific.JoeySites as JoeySites

main :: IO ()           --     _         ______`|                       ,-.__
main = defaultMain hosts --  /   \___-=O`/|O`/__|                      (____.'
  {- Propellor            -- \          / | /    )          _.-"-._
     Deployed -}          --  `/-==__ _/__|/__=-|          (       \_
hosts :: [Host]          --   *             \ | |           '--------'
hosts =                 --                  (o)  `
	[ darkstar
	, dragon
	, oyster
	, orca
	, honeybee
	, kite
	, beaver
	, sow
	, mouse
	, peregrine
	, pell
	] ++ monsters

darkstar :: Host
darkstar = host "darkstar.kitenet.net" $ props
	& osDebian Unstable X86_64
	& ipv6 "2001:4830:1600:187::2"
	& Hostname.sane
	& Hostname.mailname
	& Apt.serviceInstalledRunning "swapspace"
	& Laptop.powertopAutoTuneOnBoot
	& Laptop.trimSSD
	& Grub.cmdline_Linux_default "i915.enable_psr=1"
	! Grub.cmdline_Linux_default "quiet"
	& User.hasGroup (User "joey") (Group "dialout")

	& JoeySites.dkimMilter
	& JoeySites.postfixSaslPasswordClient
	-- & JoeySites.alarmClock "*-*-* 7:30" (User "joey")
	--	"/usr/bin/timeout 45m /home/joey/bin/goodmorning"
	& JoeySites.laptopSoftware
	& JoeySites.userDirHtml
	& Ssh.userKeys (User "joey") hostContext
		[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICfFntnesZcYz2B2T41ay45igfckXRSh5uVffkuCQkLv joey@darkstar")
		]
	& imageBuiltFor honeybee
		(RawDiskImage "/srv/honeybee.img")
		(Debootstrapped mempty)

dragon :: Host
dragon = host "dragon.kitenet.net" $ props
	& ipv6 "2001:4830:1600:187::2"
	& JoeySites.dkimMilter
	& JoeySites.postfixSaslPasswordClient

oyster :: Host
oyster = host "oyster.kitenet.net" $ props
	& standardSystem (Stable "buster") X86_64
		["Unreliable server. Anything here may be lost at any time!" ]
	& ipv4 "45.138.157.89"

	& User.hasPassword (User "root")
	& Apt.unattendedUpgrades

orca :: Host
orca = host "orca.kitenet.net" $ props
	& standardSystem Unstable X86_64 [ "Main git-annex build box." ]
	& ipv4 "138.38.108.179"

	& Apt.unattendedUpgrades
	& Postfix.satellite
	& Apt.serviceInstalledRunning "ntp"
	& Systemd.persistentJournal

	& Systemd.nspawned (GitAnnexBuilder.autoBuilderContainer
		GitAnnexBuilder.standardAutoBuilder
		Unstable X86_64 Nothing (Cron.Times "15 * * * *") "2h")
	& Systemd.nspawned (GitAnnexBuilder.autoBuilderContainer
		GitAnnexBuilder.standardAutoBuilder
		Unstable X86_32 Nothing (Cron.Times "30 * * * *") "2h")
	& Systemd.nspawned (GitAnnexBuilder.autoBuilderContainer
		GitAnnexBuilder.stackAutoBuilder
		(Stable "jessie") X86_32 (Just "ancient") (Cron.Times "45 * * * *") "2h")
	& Systemd.nspawned (GitAnnexBuilder.autoBuilderContainer
		GitAnnexBuilder.standardAutoBuilder
		Testing ARM64 Nothing (Cron.Times "1 * * * *") "4h")

honeybee :: Host
honeybee = host "honeybee.kitenet.net" $ props
	& standardSystem Testing ARMHF
		[ "Home router and arm git-annex build box." ]
	& Apt.removed ["rsyslog"]
	
	& cubietech_Cubietruck
	& hasPartition
		( partition EXT3
			`mountedAt` "/"
			`setSize` MegaBytes 16000
		)
	& JoeySites.cubieTruckOneWire
	& Systemd.persistentJournal
	& Apt.installed ["firmware-atheros"]
	& Apt.serviceInstalledRunning "ntp" -- no hardware clock
	& bootstrappedFrom GitRepoOutsideChroot
	& Ssh.hostKeys hostContext
		[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIS/hDYq1MAxfOBf49htym3BOYlx4Gk9SDpiHjv7u6IC")
		]
	& Ssh.userKeys (User "joey") hostContext
		[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAYgEgsDmN26goPBGPN0HIvtkZfxlc996nPfBPDWxGuh")
		]

	& JoeySites.house
		(User "joey")
		hosts
		(Context "house.joeyh.name")
		(SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMAmVYddg/RgCbIj+cLcEiddeFXaYFnbEJ3uGj9G/EyV joey@honeybee")
	& JoeySites.homeRouter
	& JoeySites.homeNAS
	& Apt.installed ["mtr-tiny", "iftop", "screen"]
	-- Currently manually building the xr_usb_serial module.
	& Apt.installed ["linux-headers-armmp-lpae"]
	& Postfix.satellite

	& check (not <$> hasContainerCapability Systemd.FilesystemContained) 
		(setupRevertableProperty autobuilder)
	-- In case compiler needs more than available ram
	& Apt.serviceInstalledRunning "swapspace"
  where
	autobuilder = Systemd.nspawned $ GitAnnexBuilder.autoBuilderContainer
		(GitAnnexBuilder.armAutoBuilder GitAnnexBuilder.standardAutoBuilder)
		Testing ARMEL Nothing (Cron.Times "15 15 * * *") "10h"

-- This is not a complete description of kite, since it's a
-- multiuser system with eg, user passwords that are not deployed
-- with propellor.
kite :: Host
kite = host "kite.kitenet.net" $ props
	& standardSystemUnhardened Testing X86_64 [ "Welcome to kite!" ]
	& ipv4 "66.228.36.95"
	& ipv6 "2600:3c03::f03c:91ff:fe73:b0d2"
	& alias "kitenet.net"
	& Ssh.hostKeys (Context "kitenet.net")
		[ (SshDsa, "ssh-dss AAAAB3NzaC1kc3MAAACBAO9tnPUT4p+9z7K6/OYuiBNHaij4Nzv5YVBih1vMl+ALz0gYAj8RWJzXmqp5buFAyfgOoLw+H9s1bBS01Sy3i07Dm6cx1fWG4RXL/E/3w1tavX99GD2bBxDBu890ebA5Tp+eFRJkS9+JwSvFiF6CP7NbVjifCagoUO56Ig048RwDAAAAFQDPY2xM3q6KwsVQliel23nrd0rV2QAAAIEAga3hj1hL00rYPNnAUzT8GAaSP62S4W68lusErH+KPbsMwFBFY/Ib1FVf8k6Zn6dZLh/HH/RtJi0JwdzPI1IFW+lwVbKfwBvhQ1lw9cH2rs1UIVgi7Wxdgfy8gEWxf+QIqn62wG+Ulf/HkWGvTrRpoJqlYRNS/gnOWj9Z/4s99koAAACBAM/uJIo2I0nK15wXiTYs/NYUZA7wcErugFn70TRbSgduIFH6U/CQa3rgHJw9DCPCQJLq7pwCnFH7too/qaK+czDk04PsgqV0+Jc7957gU5miPg50d60eJMctHV4eQ1FpwmGGfXxRBR9k2ZvikWYatYir3L6/x1ir7M0bA9IzNU45")
		, (SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEA2QAJEuvbTmaN9ex9i9bjPhMGj+PHUYq2keIiaIImJ+8mo+yKSaGUxebG4tpuDPx6KZjdycyJt74IXfn1voGUrfzwaEY9NkqOP3v6OWTC3QeUGqDCeJ2ipslbEd9Ep9XBp+/ldDQm60D0XsIZdmDeN6MrHSbKF4fXv1bqpUoUILk=")
		, (SshEcdsa, "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLF+dzqBJZix+CWUkAd3Bd3cofFCKwHMNRIfwx1G7dL4XFe6fMKxmrNetQcodo2edyufwoPmCPr3NmnwON9vyh0=")
		, (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFZftKMnH/zH29BHMKbcBO4QsgTrstYFVhbrzrlRzBO3")
		]

	& Network.preserveStatic "eth0" `requires` Network.cleanInterfacesFile
	& Apt.installed ["linux-image-amd64"]
	& Apt.serviceInstalledRunning "swapspace"
	& Linode.serialGrub
	& Linode.locateEnabled
	& Apt.unattendedUpgrades
	& Systemd.installed
	& Systemd.persistentJournal
	& Journald.systemMaxUse "500MiB"
	& Ssh.passwordAuthentication True
	& Fail2Ban.installed -- since ssh password authentication is allowed
	-- Allow ssh -R to forward ports via kite
	& Ssh.setSshdConfig "GatewayPorts" "clientspecified"
	& Apt.serviceInstalledRunning "ntp"
	& "/etc/timezone" `File.hasContent` ["US/Eastern"]
	
	& Borg.backup "/" (JoeySites.rsyncNetBorgRepo "kite.borg" []) Cron.Daily
		[ "--exclude=/proc/*"
		, "--exclude=/sys/*"
		, "--exclude=/run/*"
		, "--exclude=/mnt/*"
		, "--exclude=/tmp/*"
		, "--exclude=/var/tmp/*"
		, "--exclude=/var/cache/*"
		, "--exclude=/var/lib/swapspace/*"
		, "--exclude=/var/lib/container/*"
		, "--exclude=/home/joey/lib"
		-- These directories are backed up and restored separately.
		, "--exclude=/srv/git"
		]
		[ Borg.KeepDays 7
		, Borg.KeepWeeks 4
		, Borg.KeepMonths 3
		]
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")
		`requires` Ssh.userKeys (User "root")
			(Context "kite.kitenet.net")
			[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKyciu8E8qspcO8lnTSUBAxUdYfmk7FaFlvT5yxUaP+0 root@kite")
			]

	& alias "smtp.kitenet.net"
	& alias "imap.kitenet.net"
	& alias "pop.kitenet.net"
	& alias "mail.kitenet.net"
	& JoeySites.kiteMailServer

	& JoeySites.legacyWebSites
	& File.ownerGroup "/srv/web" (User "joey") (Group "joey")
	& Apt.installed ["analog"]

	& alias "git.kitenet.net"
	& alias "git.joeyh.name"
	& JoeySites.gitServer hosts

	& JoeySites.downloads
	& JoeySites.gitAnnexDistributor
	& JoeySites.tmp

	& Apt.installed
		[ "git-annex", "myrepos"
		, "build-essential", "make"
		, "rss2email", "archivemail"
		, "devscripts"
		-- Some users have zsh as their login shell.
		, "zsh"
		]

	& alias "znc.kitenet.net"
	& JoeySites.ircBouncer

	& alias "kgb.kitenet.net"
	& JoeySites.kgbServer
	
	& Systemd.nspawned ancientKitenet
	
	& alias "podcatcher.kitenet.net"
	& JoeySites.podcatcher

	& alias "ns4.kitenet.net"
	& myDnsPrimary "kitenet.net"
		[ (RelDomain "mouse-onion", CNAME $ AbsDomain "htieo6yu2qtcn2j3.onion")
		, (RelDomain "beaver-onion", CNAME $ AbsDomain "tl4xsvaxryjylgxs.onion")
		, (RelDomain "peregrine-onion", CNAME $ AbsDomain "rsdwvaabir6ty2kdzblq7wdda26ib4fuc6hzxzwum75jbn6thqbojvid.onion")
		, (RelDomain "sow-onion", CNAME $ AbsDomain "urt4g2tq32qktgtp.onion")
		]
	& myDnsPrimary "joeyh.name" []
	& myDnsPrimary "ikiwiki.info" []
	& myDnsPrimary "olduse.net"
		[ (RelDomain "article", CNAME $ AbsDomain "virgil.koldfront.dk")
		]
	! myDnsPrimary "quarantimer.app" []
	& alias "ns4.branchable.com"
	& branchableSecondary
	-- Use its own name server (amoung other things this avoids
	-- spamassassin URIBL_BLOCKED.
	& "/etc/resolv.conf" `File.hasContent`
		[ "nameserver 127.0.0.1"
		, "domain kitenet.net"
		, "search kitenet.net"
		]
	
	& alias "debug-me.joeyh.name"
	& Apt.installed ["debug-me", "debug-me-server"]
	& Systemd.enabled "debug-me"

	-- testing
	& Apache.httpsVirtualHost "letsencrypt.joeyh.name" "/var/www/html"
		(LetsEncrypt.AgreeTOS (Just "id@joeyh.name"))
	& alias "letsencrypt.joeyh.name"

beaver :: Host
beaver = host "beaver.kitenet.net" $ props
	& Apt.installed ["ssh"]
	& Ssh.hostPubKey SshDsa "ssh-dss AAAAB3NzaC1kc3MAAACBAIrLX260fY0Jjj/p0syNhX8OyR8hcr6feDPGOj87bMad0k/w/taDSOzpXe0Wet7rvUTbxUjH+Q5wPd4R9zkaSDiR/tCb45OdG6JsaIkmqncwe8yrU+pqSRCxttwbcFe+UU+4AAcinjVedZjVRDj2rRaFPc9BXkPt7ffk8GwEJ31/AAAAFQCG/gOjObsr86vvldUZHCteaJttNQAAAIB5nomvcqOk/TD07DLaWKyG7gAcW5WnfY3WtnvLRAFk09aq1EuiJ6Yba99Zkb+bsxXv89FWjWDg/Z3Psa22JMyi0HEDVsOevy/1sEQ96AGH5ijLzFInfXAM7gaJKXASD7hPbVdjySbgRCdwu0dzmQWHtH+8i1CMVmA2/a5Y/wtlJAAAAIAUZj2US2D378jBwyX1Py7e4sJfea3WSGYZjn4DLlsLGsB88POuh32aOChd1yzF6r6C2sdoPBHQcWBgNGXcx4gF0B5UmyVHg3lIX2NVSG1ZmfuLNJs9iKNu4cHXUmqBbwFYQJBvB69EEtrOw4jSbiTKwHFmqdA/mw1VsMB+khUaVw=="
	& Tor.installed
	& Tor.hiddenServiceAvailable "ssh" (Port 22)

sow :: Host
sow = host "sow.kitenet.net" $ props
	& Apt.installed ["ssh"]
	& Ssh.hostPubKey SshDsa "ssh-dss AAAAB3NzaC1kc3MAAACBAIrLX260fY0Jjj/p0syNhX8OyR8hcr6feDPGOj87bMad0k/w/taDSOzpXe0Wet7rvUTbxUjH+Q5wPd4R9zkaSDiR/tCb45OdG6JsaIkmqncwe8yrU+pqSRCxttwbcFe+UU+4AAcinjVedZjVRDj2rRaFPc9BXkPt7ffk8GwEJ31/AAAAFQCG/gOjObsr86vvldUZHCteaJttNQAAAIB5nomvcqOk/TD07DLaWKyG7gAcW5WnfY3WtnvLRAFk09aq1EuiJ6Yba99Zkb+bsxXv89FWjWDg/Z3Psa22JMyi0HEDVsOevy/1sEQ96AGH5ijLzFInfXAM7gaJKXASD7hPbVdjySbgRCdwu0dzmQWHtH+8i1CMVmA2/a5Y/wtlJAAAAIAUZj2US2D378jBwyX1Py7e4sJfea3WSGYZjn4DLlsLGsB88POuh32aOChd1yzF6r6C2sdoPBHQcWBgNGXcx4gF0B5UmyVHg3lIX2NVSG1ZmfuLNJs9iKNu4cHXUmqBbwFYQJBvB69EEtrOw4jSbiTKwHFmqdA/mw1VsMB+khUaVw=="
	& Tor.installed
	& Tor.hiddenServiceAvailable "ssh" (Port 22)

mouse :: Host
mouse = host "mouse.kitenet.net" $ props
	& ipv4 "67.223.19.96"
	& Apt.installed ["ssh"]
	& Tor.installed
	& Tor.hiddenServiceAvailable "ssh" (Port 22)

peregrine :: Host
peregrine = host "peregrine.kitenet.net" $ props
	& Apt.installed ["ssh", "screen", "variety", "git-annex"]
	& Tor.installed
	& Tor.hiddenServiceAvailable "ssh" (Port 22)
	& LightDM.autoLogin (User "desktop")

-- Branchable is not completely deployed with propellor yet.
pell :: Host
pell = host "pell.branchable.com" $ props
	& alias "branchable.com"
	& ipv4 "66.228.46.55"
	& ipv6 "2600:3c03::f03c:91ff:fedf:c0e5"

	-- All the websites I host at branchable that don't use
	-- branchable.com dns.
	& alias "olduse.net"
	& alias "www.olduse.net"
	& alias "www.kitenet.net"
	& alias "joeyh.name"
	& alias "www.joeyh.name"
	& alias "campaign.joeyh.name"
	& alias "ikiwiki.info"
	& alias "www.ikiwiki.info"
	& alias "git.ikiwiki.info"
	& alias "l10n.ikiwiki.info"
	& alias "dist-bugs.kitenet.net"
	& alias "family.kitenet.net"

	& osDebian (Stable "buster") X86_32
	& Apt.installed ["linux-image-686-pae"]
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.unattendedUpgrades
	& Branchable.server hosts
	& Linode.serialGrub



       --'                        __|II|      ,.
     ----                      __|II|II|__   (  \_,/\
--'-------'\o/-'-.-'-.-'-.- __|II|II|II|II|___/   __/ -'-.-'-.-'-.-'-.-'-.-'-
-------------------------- |   [Containers]      / --------------------------
-------------------------- :                    / ---------------------------
--------------------------- \____, o          ,' ----------------------------
---------------------------- '--,___________,'  -----------------------------

-- Exhibit: kite's 90's website on port 1994.
ancientKitenet :: Systemd.Container
ancientKitenet = Systemd.debContainer "ancient-kitenet" $ props
	& standardContainer (Stable "buster")
	& alias hn
	& Git.cloned (User "root") "git://kitenet-net.branchable.com/" "/var/www/html"
		(Just "remotes/origin/old-kitenet.net")
	& Apache.installed
	& Apache.listenPorts [p]
	& Apache.virtualHost hn p "/var/www/html"
	& Apache.siteDisabled "000-default"
  where
	p = Port 1994
	hn = "ancient.kitenet.net"

type Motd = [String]

-- This is my standard system setup.
standardSystem :: DebianSuite -> Architecture -> Motd -> Property (HasInfo + Debian)
standardSystem suite arch motd =
	standardSystemUnhardened suite arch motd
		`before` Ssh.noPasswords

standardSystemUnhardened :: DebianSuite -> Architecture -> Motd -> Property (HasInfo + Debian)
standardSystemUnhardened suite arch motd = propertyList "standard system" $ props
	& osDebian suite arch
	& Hostname.sane
	& Hostname.mailname
	& Hostname.searchDomain
	& Locale.available "en_US.UTF-8"
	& File.hasContent "/etc/motd" ("":motd++[""])
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.cacheCleaned
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh", "mosh"]
	& GitHome.installedFor (User "root")
	& User.hasSomePassword (User "root")
	& User.accountFor (User "joey")
	& User.hasSomePassword (User "joey")
	& Sudo.enabledFor (User "joey")
	& GitHome.installedFor (User "joey")
	& Apt.installed ["vim", "screen", "less"]
	& Cron.runPropellor (Cron.Times "30 * * * *")
	-- I use postfix, or no MTA.
	& JoeySites.noExim

-- This is my standard container setup, Featuring automatic upgrades.
standardContainer :: DebianSuite -> Property (HasInfo + Debian)
standardContainer suite = propertyList "standard container" $ props
	& osDebian suite X86_64
	-- Do not want to run mail daemon inside a random container..
	& JoeySites.noExim
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.unattendedUpgrades
	& Apt.cacheCleaned

branchableSecondary :: RevertableProperty (HasInfo + DebianLike) DebianLike
branchableSecondary = Dns.secondaryFor ["branchable.com"] hosts "branchable.com"

-- Currently using kite (ns4) as primary with gandi as secondary
-- kite handles all mail.
myDnsPrimary :: Domain -> [(BindDomain, Record)] -> RevertableProperty (HasInfo + DebianLike) DebianLike
myDnsPrimary domain extras = Dns.signedPrimary (Weekly Nothing) hosts domain
	(Dns.mkSOA "ns4.kitenet.net" 100) $
	[ (RootDomain, NS $ AbsDomain "ns4.kitenet.net")
	, (RootDomain, NS $ AbsDomain "ns6.gandi.net")
	, (RootDomain, MX 0 $ AbsDomain "kitenet.net")
	, (RootDomain, TXT "v=spf1 a a:kitenet.net ~all")
	, JoeySites.domainKey
	] ++ extras

-- Systems I don't manage with propellor,
-- but do want to track their public keys etc.
monsters :: [Host]
monsters =
	[ host "usw-s002.rsync.net" $ props
		& Ssh.hostPubKey SshEd25519 "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB7yTEBGfQYdwG/oeL+U9XPMIh/dW7XNs9T+M79YIOrd"
	, host "ns6.gandi.net" $ props
		& ipv4 "217.70.177.40"
	]



                          --                                o
                          --             ___                 o              o
                       {-----\          / o \              ___o            o
                       {      \    __   \   /   _        (X___>--         __o
  _____________________{ ______\___  \__/ | \__/ \____                  |X__>
 <                                  \___//|\\___/\     \____________   _
  \                                  ___/ | \___    # #             \ (-)
   \    O      O      O             #     |     \ #                  >=)
    \______________________________# #   /       #__________________/ (-}


