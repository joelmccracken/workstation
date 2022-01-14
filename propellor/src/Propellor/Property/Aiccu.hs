{-# LANGUAGE TypeFamilies #-}

-- | Maintainer: Jelmer Vernooij <jelmer@samba.org>

module Propellor.Property.Aiccu (
	installed,
	restarted,
	confPath,
	UserName,
	TunnelId,
	hasConfig,
) where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.File as File

installed :: Property DebianLike
installed = Apt.installed ["aiccu"]

restarted :: Property DebianLike
restarted = Service.restarted "aiccu"

confPath :: FilePath
confPath = "/etc/aiccu.conf"

type TunnelId = String

config :: UserName -> TunnelId -> PrivData -> [File.Line]
config u t p = 
	[ "protocol tic"
	, "server tic.sixxs.net"
	, "username " ++ u
	, "password " ++ privDataVal p
	, "ipv6_interface sixxs"
	, "tunnel_id " ++ t
	, "daemonize true"
	, "automatic true"
	, "requiretls true"
	, "makebeats true"
	]

-- | Configures an ipv6 tunnel using sixxs.net, with the given TunneId
-- and sixx.net UserName.
hasConfig :: TunnelId -> UserName -> Property (HasInfo + DebianLike)
hasConfig t u = prop `onChange` restarted
  where
  	prop :: Property (HasInfo + UnixLike)
	prop = withSomePrivData [(Password (u++"/"++t)), (Password u)] (Context "aiccu") $ \getpassword ->
		property' "aiccu configured" $ \w -> getpassword $ ensureProperty w . go
	go (Password u', p) = confPath `File.hasContentProtected` config u' t p
	go (f, _) = error $ "Unexpected type of privdata: " ++ show f
