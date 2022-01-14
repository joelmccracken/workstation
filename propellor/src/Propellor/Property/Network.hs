module Propellor.Property.Network where

import Propellor.Base
import Propellor.Property.File

import Data.Char

type Interface = String

-- | Options to put in a stanza of an ifupdown interfaces file.
type InterfaceOptions = [(String, String)]

-- | Stanza of an ifupdown interfaces file, with header lines and options.
type InterfaceStanza = ([String], InterfaceOptions)

ifUp :: Interface -> Property DebianLike
ifUp iface = tightenTargets $ cmdProperty "ifup" [iface]
	`assume` MadeChange

-- | Resets /etc/network/interfaces to a clean and empty state,
-- containing just the standard loopback interface, and with
-- interfacesD enabled.
--
-- This can be used as a starting point to defining other interfaces.
--
-- No interfaces are brought up or down by this property.
cleanInterfacesFile :: Property DebianLike
cleanInterfacesFile = interfaceFileContains interfacesFile
	[ "source-directory interfaces.d"
	, ""
	, "# The loopback network interface"
	, "auto lo"
	, "iface lo inet loopback"
	]
	[]
	`describe` ("clean " ++ interfacesFile)

-- | Configures an interface to get its address via dhcp.
dhcp :: Interface -> Property DebianLike
dhcp iface = dhcp' iface mempty

dhcp' :: Interface -> InterfaceOptions -> Property DebianLike
dhcp' iface options = interfaceFileContains (interfaceDFile iface)
	[ "auto " ++ iface
	, "iface " ++ iface ++ " inet dhcp"
	] options
	`describe` ("dhcp " ++ iface)
	`requires` interfacesDEnabled

newtype Gateway = Gateway IPAddr

-- | Configures an interface with a static address and gateway.
static :: Interface -> IPAddr -> Maybe Gateway -> Property DebianLike
static iface addr gateway = static' iface addr gateway mempty

static' :: Interface -> IPAddr -> Maybe Gateway -> InterfaceOptions -> Property DebianLike
static' iface addr gateway options =
	static'' iface [(addr, gateway, options)]

-- | Configures an interface with several stanzas (IPv4 and IPv6 for example).
static'' :: Interface -> [(IPAddr, Maybe Gateway, InterfaceOptions)] -> Property DebianLike
static'' iface confs =
	interfaceFileContains' (interfaceDFile iface) stanzas
	`describe` ("static IP address for " ++ iface)
	`requires` interfacesDEnabled
  where
	stanzas = map stanza confs
	stanza (addr, gateway, options) = (headerlines addr, options' addr gateway options)
	headerlines addr =
		[ "auto " ++ iface
		, "iface " ++ iface ++ " " ++ (inet addr) ++ " static"
		]
	options' addr gateway options = catMaybes
		[ Just $ ("address", val addr)
		, case gateway of
			Just (Gateway gaddr) ->
				Just ("gateway", val gaddr)
			Nothing -> Nothing
		] ++ options
	inet addr = case addr of
		IPv4 _ -> "inet"
		IPv6 _ -> "inet6"

-- | Writes a static interface file for the specified interface
-- to preserve its current configuration.
--
-- The interface has to be up already. It could have been brought up by
-- DHCP, or by other means. The current ipv4 addresses
-- and routing configuration of the interface are written into the file.
--
-- If the interface file already exists, this property does nothing,
-- no matter its content.
--
-- (ipv6 addresses are not included because it's assumed they come up
-- automatically in most situations.)
preserveStatic :: Interface -> Property DebianLike
preserveStatic iface = tightenTargets $ 
	check (not <$> doesFileExist f) setup
		`describe` desc
		`requires` interfacesDEnabled
  where
	f = interfaceDFile iface
	desc = "static " ++ iface
	setup :: Property DebianLike
	setup = property' desc $ \o -> do
		ls <- liftIO $ lines <$> readProcess "ip"
			["-o", "addr", "show", iface, "scope", "global"]
		stanzas <- liftIO $ concat <$> mapM mkstanza ls
		ensureProperty o $ hasContent f $ ("auto " ++ iface) : stanzas
	mkstanza ipline = case words ipline of
		-- Note that the IP address is written CIDR style, so
		-- the netmask does not need to be specified separately.
		(_:iface':"inet":addr:_) | iface' == iface -> do
			gw <- getgateway
			return $ catMaybes
				[ Just $ "iface " ++ iface ++ " inet static"
				, Just $ "\taddress " ++ addr
				, ("\tgateway " ++) <$> gw
				]
		_ -> return []
	getgateway = do
		rs <- lines <$> readProcess "ip"
			["route", "show", "scope", "global", "dev", iface]
		return $ case words <$> headMaybe rs of
			Just ("default":"via":gw:_) -> Just gw
			_ -> Nothing

-- | 6to4 ipv6 connection, should work anywhere
ipv6to4 :: Property DebianLike
ipv6to4 = tightenTargets $ interfaceFileContains (interfaceDFile "sit0")
	[ "auto sit0"
	, "iface sit0 inet6 static"
	]
	[ ("address", "2002:5044:5531::1")
	, ("netmask", "64")
	, ("gateway", "::192.88.99.1")
	]
	`describe` "ipv6to4"
	`requires` interfacesDEnabled
	`onChange` ifUp "sit0"

interfacesFile :: FilePath
interfacesFile = "/etc/network/interfaces"

-- | A file in the interfaces.d directory.
interfaceDFile :: Interface -> FilePath
interfaceDFile i = "/etc/network/interfaces.d" </> escapeInterfaceDName i

-- | /etc/network/interfaces.d/ files have to match -- ^[a-zA-Z0-9_-]+$
-- see "man 5 interfaces"
escapeInterfaceDName :: Interface -> FilePath
escapeInterfaceDName = filter (\c -> isAscii c && (isAlphaNum c || c `elem` "_-"))

-- | Ensures that files in the the interfaces.d directory are used.
-- interfacesDEnabled :: Property DebianLike
interfacesDEnabled :: Property DebianLike
interfacesDEnabled = tightenTargets $
	containsLine interfacesFile "source-directory interfaces.d"
		`describe` "interfaces.d directory enabled"

interfaceFileContains :: FilePath -> [String] -> InterfaceOptions -> Property DebianLike
interfaceFileContains f headerlines options =
	interfaceFileContains' f [(headerlines, options)]

interfaceFileContains' :: FilePath -> [InterfaceStanza] -> Property DebianLike
interfaceFileContains' f stanzas = tightenTargets $ hasContent f $
	warning : concatMap stanza stanzas
  where
	stanza (headerlines, options) = headerlines ++ map fmt options
	fmt (k, v) = "\t" ++ k ++ " " ++ v
	warning = "# Deployed by propellor, do not edit."
