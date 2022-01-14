{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Property.Service where

import Propellor.Base
import Propellor.Types.Info
import qualified Propellor.Property.File as File

type ServiceName = String

-- | Ensures that a service is running. Does not ensure that
-- any package providing that service is installed. See
-- Apt.serviceInstalledRunning
--
-- Note that due to the general poor state of init scripts, the best
-- we can do is try to start the service, and if it fails, assume
-- this means it's already running.
running :: ServiceName -> Property DebianLike
running = signaled "start" "running"

restarted :: ServiceName -> Property DebianLike
restarted = signaled "restart" "restarted"

reloaded :: ServiceName -> Property DebianLike
reloaded = signaled "reload" "reloaded"

signaled :: String -> Desc -> ServiceName -> Property DebianLike
signaled cmd desc svc = check (not <$> servicesDisabled) $
	tightenTargets $ p `describe` (desc ++ " " ++ svc)
  where
	p = scriptProperty ["service " ++ shellEscape svc ++ " " ++ cmd ++ " >/dev/null 2>&1 || true"]
		`assume` NoChange

-- | This property prevents daemons and other services from being started,
-- which is often something you want to prevent when building a chroot.
--
-- When this is set, `running` and `restarted` will not start services.
--
-- On Debian this installs a </usr/sbin/policy-rc.d> script to further
-- prevent any packages that get installed from starting daemons.
-- Reverting the property removes the script.
noServices :: RevertableProperty (HasInfo + UnixLike) UnixLike
noServices = (setup `setInfoProperty` toInfo (InfoVal NoServices)) <!> teardown
  where
	f = "/usr/sbin/policy-rc.d"
	script = [ "#!/bin/sh", "exit 101" ]
	setup = combineProperties "no services started" $ toProps
		[ File.hasContent f script
		, File.mode f (combineModes (readModes ++ executeModes))
		]
	teardown = File.notPresent f

-- | Check if the noServices property is in effect.
servicesDisabled :: Propellor Bool
servicesDisabled = isJust . fromInfoVal
	<$> (askInfo :: Propellor (InfoVal NoServices))

data NoServices = NoServices deriving (Eq, Show, Typeable)
