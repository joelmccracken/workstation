{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Propellor.Property.Systemd (
	-- * Services
	ServiceName,
	started,
	stopped,
	enabled,
	disabled,
	masked,
	running,
	restarted,
	networkd,
	journald,
	logind,
	escapePath,
	-- * Configuration
	installed,
	Option,
	configured,
	daemonReloaded,
	-- * Journal
	persistentJournal,
	journaldConfigured,
	-- * Logind
	logindConfigured,
	killUserProcesses,
	-- * Containers and machined
	machined,
	MachineName,
	Container,
	container,
	debContainer,
	nspawned,
	-- * Container configuration
	containerCfg,
	resolvConfed,
	linkJournal,
	privateNetwork,
	module Propellor.Types.Container,
	Proto(..),
	Publishable,
	publish,
	Bindable,
	bind,
	bindRo,
) where

import Propellor.Base
import Propellor.Types.Chroot
import Propellor.Types.Container
import Propellor.Container
import Propellor.Types.Info
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import Propellor.Property.Systemd.Core
import Utility.Split

import Data.List
import Data.Char
import qualified Data.Map as M
import Text.Printf

type ServiceName = String

type MachineName = String

data Container = Container MachineName Chroot.Chroot Host
	deriving (Show)

instance IsContainer Container where
	containerProperties (Container _ _ h) = containerProperties h
	containerInfo (Container _ _ h) = containerInfo h
	setContainerProperties (Container n c h) ps = Container n c (setContainerProperties h ps)

-- | Starts a systemd service.
--
-- Note that this does not configure systemd to start the service on boot,
-- it only ensures that the service is currently running.
started :: ServiceName -> Property Linux
started n = tightenTargets $ cmdProperty "systemctl" ["start", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " started")

-- | Stops a systemd service.
stopped :: ServiceName -> Property Linux
stopped n = tightenTargets $ cmdProperty "systemctl" ["stop", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " stopped")

-- | Enables a systemd service.
--
-- This does not ensure the service is started, it only configures systemd
-- to start it on boot.
enabled :: ServiceName -> Property Linux
enabled n = tightenTargets $ cmdProperty "systemctl" ["enable", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " enabled")

-- | Disables a systemd service.
disabled :: ServiceName -> Property Linux
disabled n = tightenTargets $ cmdProperty "systemctl" ["disable", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " disabled")

-- | Masks a systemd service.
masked :: ServiceName -> RevertableProperty Linux Linux
masked n = systemdMask <!> systemdUnmask
  where
	systemdMask = tightenTargets $ cmdProperty "systemctl" ["mask", n]
		`assume` NoChange
		`describe` ("service " ++ n ++ " masked")
	systemdUnmask = tightenTargets $ cmdProperty "systemctl" ["unmask", n]
		`assume` NoChange
		`describe` ("service " ++ n ++ " unmasked")

-- | Ensures that a service is both enabled and started
running :: ServiceName -> Property Linux
running n = started n `requires` enabled n

-- | Restarts a systemd service.
restarted :: ServiceName -> Property Linux
restarted n = tightenTargets $ cmdProperty "systemctl" ["restart", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " restarted")

-- | The systemd-networkd service.
networkd :: ServiceName
networkd = "systemd-networkd"

-- | The systemd-journald service.
journald :: ServiceName
journald = "systemd-journald"

-- | The systemd-logind service.
logind :: ServiceName
logind = "systemd-logind"

-- | Enables persistent storage of the journal.
persistentJournal :: Property DebianLike
persistentJournal = check (not <$> doesDirectoryExist dir) $
	combineProperties "persistent systemd journal" $ props
		& cmdProperty "install" ["-d", "-g", "systemd-journal", dir]
			`assume` MadeChange
		& Apt.installed ["acl"]
		& cmdProperty "setfacl" ["-R", "-nm", "g:adm:rx,d:g:adm:rx", dir]
			`assume` MadeChange
		& started "systemd-journal-flush"
  where
	dir = "/var/log/journal"

type Option = String

-- | Ensures that an option is configured in one of systemd's config files.
-- Does not ensure that the relevant daemon notices the change immediately.
--
-- This assumes that there is only one [Header] per file, which is
-- currently the case for files like journald.conf and system.conf.
-- And it assumes the file already exists with
-- the right [Header], so new lines can just be appended to the end.
configured :: FilePath -> Option -> String -> Property Linux
configured cfgfile option value = tightenTargets $ combineProperties desc $ props
	& File.fileProperty desc (mapMaybe removeother) cfgfile
	& File.containsLine cfgfile line
  where
	setting = option ++ "="
	line = setting ++ value
	desc = cfgfile ++ " " ++ line
	removeother l
		| setting `isPrefixOf` l && l /= line = Nothing
		| otherwise = Just l

-- | Causes systemd to reload its configuration files.
daemonReloaded :: Property Linux
daemonReloaded = tightenTargets $ cmdProperty "systemctl" ["daemon-reload"]
	`assume` NoChange

-- | Configures journald, restarting it so the changes take effect.
journaldConfigured :: Option -> String -> Property Linux
journaldConfigured option value =
	configured "/etc/systemd/journald.conf" option value
		`onChange` restarted journald

-- | Configures logind, restarting it so the changes take effect.
logindConfigured :: Option -> String -> Property Linux
logindConfigured option value =
	configured "/etc/systemd/logind.conf" option value
		`onChange` restarted logind

-- | Configures whether leftover processes started from the
-- user's login session are killed after the user logs out.
--
-- The default configuration varies depending on the version of systemd.
--
-- Revert the property to ensure that screen sessions etc keep running:
--
-- >	! killUserProcesses
killUserProcesses :: RevertableProperty Linux Linux
killUserProcesses = set "yes" <!> set "no"
  where
	set = logindConfigured "KillUserProcesses"

-- | Ensures machined and machinectl are installed
machined :: Property Linux
machined = installeddebian `pickOS` assumeinstalled
  where
	installeddebian :: Property DebianLike
	installeddebian = withOS "machined installed" $ \w o ->
		case o of
			-- Split into separate debian package since systemd 225.
			(Just (System (Debian _ suite) _))
				| not (isStable suite) || suite /= (Stable "jessie") ->
					ensureProperty w $ Apt.installed ["systemd-container"]
			_ -> noChange
	assumeinstalled :: Property Linux
	assumeinstalled = doNothing

-- | Defines a container with a given machine name,
-- and how to create its chroot if not already present.
--
-- Properties can be added to configure the Container. At a minimum,
-- add a property such as `osDebian` to specify the operating system
-- to bootstrap.
--
-- > container "webserver" $ \d -> Chroot.debootstrapped mempty d $ props
-- >    & osDebian Unstable X86_64
-- >    & Apt.installedRunning "apache2"
-- >    & ...
container :: MachineName -> (FilePath -> Chroot.Chroot) -> Container
container name mkchroot =
	let c = Container name chroot h
	in setContainerProps c $ containerProps c
		&^ linkJournal
  where
	chroot = mkchroot (containerDir name)
	h = Host name (containerProperties chroot) (containerInfo chroot)

-- | Defines a container with a given machine name, with the chroot
-- created using debootstrap.
--
-- Properties can be added to configure the Container. At a minimum,
-- add a property such as `osDebian` to specify the operating system
-- to bootstrap.
--
-- > debContainer "webserver" $ props
-- >    & osDebian Unstable X86_64
-- >    & Apt.installedRunning "apache2"
-- >    & ...
debContainer :: MachineName -> Props metatypes -> Container
debContainer name ps = container name $ \d -> Chroot.debootstrapped mempty d ps

-- | Runs a container using systemd-nspawn.
--
-- A systemd unit is set up for the container, so it will automatically
-- be started on boot.
--
-- Systemd is automatically installed inside the container, and will
-- communicate with the host's systemd. This allows systemctl to be used to
-- examine the status of services running inside the container.
--
-- When the host system has persistentJournal enabled, journactl can be
-- used to examine logs forwarded from the container.
--
-- Reverting this property stops the container, removes the systemd unit,
-- and deletes the chroot and all its contents.
nspawned :: Container -> RevertableProperty (HasInfo + Linux) Linux
nspawned c@(Container name (Chroot.Chroot loc builder _ _) h) =
	p `describe` ("nspawned " ++ name)
  where
	p :: RevertableProperty (HasInfo + Linux) Linux
	p = enterScript c
		`before` chrootprovisioned
		`before` nspawnService c (_chrootCfg $ fromInfo $ hostInfo h)
		`before` containerprovisioned

	-- Chroot provisioning is run in systemd-only mode,
	-- which sets up the chroot and ensures systemd and dbus are
	-- installed, but does not handle the other properties.
	chrootprovisioned = Chroot.provisioned' chroot True [FilesystemContained]

	-- Use nsenter to enter container and and run propellor to
	-- finish provisioning.
	containerprovisioned :: RevertableProperty Linux Linux
	containerprovisioned =
		tightenTargets (Chroot.propellChroot chroot (enterContainerProcess c) False containercaps)
			<!>
		doNothing

	containercaps = 
		[ FilesystemContained
		, HostnameContained
		]

	chroot = Chroot.Chroot loc builder Chroot.propagateChrootInfo h

-- | Sets up the service files for the container, using the
-- systemd-nspawn@.service template, and starts it running.
nspawnService :: Container -> ChrootCfg -> RevertableProperty Linux Linux
nspawnService (Container name _ _) cfg = setup <!> teardown
  where
	service = nspawnServiceName name
	overridedir = "/etc/systemd/system" </> nspawnServiceName name ++ ".d"
	overridefile = overridedir </> "local.conf"
	overridecontent = 
		[ "[Service]"
		, "# Reset ExecStart from the template"
		, "ExecStart="
		, "ExecStart=/usr/bin/systemd-nspawn " ++ unwords nspawnparams
		]
	nspawnparams = 
		[ "--quiet"
		, "--keep-unit"
		, "--boot"
		, "--directory=" ++ containerDir name
		, "--machine=" ++ name
		] ++ nspawnServiceParams cfg

	overrideconfigured = File.hasContent overridefile overridecontent
		`onChange` daemonReloaded
		`requires` File.dirExists overridedir

	setup :: Property Linux
	setup = started service
		`requires` enabled service
		`requires` overrideconfigured
		`requires` machined

	teardown :: Property Linux
	teardown = stopped service
		`before` disabled service
		`before` File.notPresent overridefile

nspawnServiceParams :: ChrootCfg -> [String]
nspawnServiceParams NoChrootCfg = []
nspawnServiceParams (SystemdNspawnCfg ps) =
	M.keys $ M.filter id $ M.fromList ps

-- | Installs a "enter-machinename" script that root can use to run a
-- command inside the container.
--
-- This uses nsenter to enter the container, by looking up the pid of the
-- container's init process and using its namespace.
enterScript :: Container -> RevertableProperty Linux Linux
enterScript c@(Container name _ _) =
	tightenTargets setup <!> tightenTargets teardown
  where
	setup = combineProperties ("generated " ++ enterScriptFile c) $ props
		& scriptfile `File.hasContent`
			[ "#!/usr/bin/perl"
			, "# Generated by propellor"
			, "my $pid=`machinectl show " ++ shellEscape name ++ " -p Leader | cut -d= -f2`;"
			, "chomp $pid;"
			, "if (length $pid) {"
			, "\tforeach my $var (keys %ENV) {"
			, "\t\tdelete $ENV{$var} unless $var eq 'PATH' || $var eq 'TERM';"
			, "\t}"
			, "\texec('nsenter', '-p', '-u', '-n', '-i', '-m', '-t', $pid, @ARGV);"
			, "} else {"
			, "\tdie 'container not running';"
			, "}"
			, "exit(1);"
			]
		& scriptfile `File.mode` combineModes (readModes ++ executeModes)
	teardown = File.notPresent scriptfile
	scriptfile = enterScriptFile c

enterScriptFile :: Container -> FilePath
enterScriptFile (Container name _ _ ) = "/usr/local/bin/enter-" ++ mungename name

enterContainerProcess :: Container -> [String] -> IO (CreateProcess, IO ())
enterContainerProcess c ps = pure (proc (enterScriptFile c) ps, noop)

nspawnServiceName :: MachineName -> ServiceName
nspawnServiceName name = "systemd-nspawn@" ++ name ++ ".service"

containerDir :: MachineName -> FilePath
containerDir name = "/var/lib/container" </> mungename name

mungename :: MachineName -> String
mungename = replace "/" "_"

-- | This configures how systemd-nspawn(1) starts the container,
-- by specifying a parameter, such as "--private-network", or
-- "--link-journal=guest"
--
-- When there is no leading dash, "--" is prepended to the parameter.
--
-- Reverting the property will remove a parameter, if it's present.
containerCfg :: String -> RevertableProperty (HasInfo + Linux) (HasInfo + Linux)
containerCfg p = RevertableProperty (mk True) (mk False)
  where
	mk :: Bool -> Property (HasInfo + Linux)
	mk b = tightenTargets $
		pureInfoProperty desc $
			mempty { _chrootCfg = SystemdNspawnCfg [(p', b)] }
	  where
		desc = "container configuration " ++ (if b then "" else "without ") ++ p'
	p' = case p of
		('-':_) -> p
		_ -> "--" ++ p

-- | Bind mounts </etc/resolv.conf> from the host into the container.
--
-- This is not necessary when systemd configures the container's
-- resolv.conf on its own. This used to be enabled by default, but when
-- systemd did also configure the container's resolv.conf, that could
-- modify the host's resolv.conf.
resolvConfed :: RevertableProperty (HasInfo + Linux) (HasInfo + Linux)
resolvConfed = containerCfg "bind=/etc/resolv.conf"

-- | Link the container's journal to the host's if possible.
-- (Only works if the host has persistent journal enabled.)
--
-- This property is enabled by default. Revert it to disable it.
linkJournal :: RevertableProperty (HasInfo + Linux) (HasInfo + Linux)
linkJournal = containerCfg "link-journal=try-guest"

-- | Disconnect networking of the container from the host.
privateNetwork :: RevertableProperty (HasInfo + Linux) (HasInfo + Linux)
privateNetwork = containerCfg "private-network"

class Publishable a where
	toPublish :: a -> String

instance Publishable Port where
	toPublish port = val port

instance Publishable (Bound Port) where
	toPublish v = toPublish (hostSide v) ++ ":" ++ toPublish (containerSide v)

data Proto = TCP | UDP

instance Publishable (Proto, Bound Port) where
	toPublish (TCP, fp) = "tcp:" ++ toPublish fp
	toPublish (UDP, fp) = "udp:" ++ toPublish fp

-- | Publish a port from the container to the host.
--
-- This feature was first added in systemd version 220.
--
-- This property is only needed (and will only work) if the container
-- is configured to use private networking. Also, networkd should be enabled
-- both inside the container, and on the host. For example:
--
-- > foo :: Host
-- > foo = host "foo.example.com"
-- >	& Systemd.nspawned webserver
-- > 		`requires` Systemd.running Systemd.networkd
-- >
-- > webserver :: Systemd.container
-- > webserver = Systemd.container "webserver" (Chroot.debootstrapped mempty)
-- >	& os (System (Debian Testing) X86_64)
-- >	& Systemd.privateNetwork
-- >	& Systemd.running Systemd.networkd
-- >	& Systemd.publish (Port 80 ->- Port 8080)
-- >	& Apt.installedRunning "apache2"
publish :: Publishable p => p -> RevertableProperty (HasInfo + Linux) (HasInfo + Linux)
publish p = containerCfg $ "--port=" ++ toPublish p

class Bindable a where
	toBind :: a -> String

instance Bindable FilePath where
	toBind f = f

instance Bindable (Bound FilePath) where
	toBind v = hostSide v ++ ":" ++ containerSide v

-- | Bind mount a file or directory from the host into the container.
bind :: Bindable p => p -> RevertableProperty (HasInfo + Linux) (HasInfo + Linux)
bind p = containerCfg $ "--bind=" ++ toBind p

-- | Read-only mind mount.
bindRo :: Bindable p => p -> RevertableProperty (HasInfo + Linux) (HasInfo + Linux)
bindRo p = containerCfg $ "--bind-ro=" ++ toBind p

-- | Escapes a path for inclusion in a systemd unit name,
-- the same as systemd-escape does.
escapePath :: FilePath -> String
escapePath = concatMap escape 
	. dropWhile (== '/')
	. reverse . dropWhile (== '/') . reverse
  where
	escape '/' = "-"
	escape c
		| ((isAscii c && isAlphaNum c) || c == '_') = [c]
		| otherwise = '\\' : 'x' : printf "%x" c
