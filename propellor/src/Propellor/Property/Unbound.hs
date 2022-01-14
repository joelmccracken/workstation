-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>
--
-- Properties for the Unbound caching DNS server

module Propellor.Property.Unbound
	( installed
	, restarted
	, reloaded
	, UnboundSection
	, UnboundZone
	, UnboundHost
	, UnboundSetting
	, UnboundValue
	, UnboundKey
	, ConfSection
	, ZoneType
	, cachingDnsServer
	) where

import Propellor.Base
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

import Data.List (find)


type ConfSection = String

type UnboundSetting = (UnboundKey, UnboundValue)

type UnboundSection = (ConfSection, [UnboundSetting])

type UnboundZone = (BindDomain, ZoneType)

type UnboundHost = (BindDomain, Record)

type UnboundKey = String

type UnboundValue = String

type ZoneType = String

installed :: Property DebianLike
installed = Apt.installed ["unbound"]

restarted :: Property DebianLike
restarted = Service.restarted "unbound"

reloaded :: Property DebianLike
reloaded = Service.reloaded "unbound"

dValue :: BindDomain -> String
dValue (RelDomain d) = d
dValue (AbsDomain d) = d ++ "."
dValue (RootDomain) = "@"

sectionHeader :: ConfSection -> String
sectionHeader header = header ++ ":"

config :: FilePath
config = "/etc/unbound/unbound.conf.d/propellor.conf"

-- | Provided a [UnboundSection], a [UnboundZone] and a [UnboundHost],
-- cachingDnsServer ensure unbound is configured accordingly.
--
-- Be carefull with CNAMEs, unbound is not a primary DNS server, so it will
-- resolve these by itself. For a locally served zone, you probably want A/AAAA
-- records instead.
--
-- Example property:
--
-- > cachingDnsServer
-- >      [ ("remote-control", [("control-enable", "no")]
-- >      , ("server",
-- >      	[ ("interface", "0.0.0.0")
-- >      	, ("access-control", "192.168.1.0/24 allow")
-- >      	, ("do-tcp", "no")
-- >      	])
-- >      [ (AbsDomain "example.com", "transparent")
-- >      , (AbsDomain $ reverseIP $ IPv4 "192.168.1", "static")
-- >      ]
-- >      [ (AbsDomain "example.com", Address $ IPv4 "192.168.1.2")
-- >      , (AbsDomain "myhost.example.com", Address $ IPv4 "192.168.1.2")
-- >      , (AbsDomain "myrouter.example.com", Address $ IPv4 "192.168.1.1")
-- >      , (AbsDomain "www.example.com", Address $ IPv4 "192.168.1.2")
-- >      , (AbsDomain "example.com", MX 10 "mail.example.com")
-- >      , (AbsDomain "mylaptop.example.com", Address $ IPv4 "192.168.1.2")
-- >      -- ^ connected via ethernet
-- >      , (AbsDomain "mywifi.example.com", Address $ IPv4 "192.168.2.1")
-- >      , (AbsDomain "mylaptop.example.com", Address $ IPv4 "192.168.2.2")
-- >      -- ^ connected via wifi, use round robin
-- >      , (AbsDomain "myhost.example.com", PTR $ reverseIP $ IPv4 "192.168.1.2")
-- >      , (AbsDomain "myrouter.example.com", PTR $ reverseIP $ IPv4 "192.168.1.1")
-- >      , (AbsDomain "mylaptop.example.com", PTR $ reverseIP $ IPv4 "192.168.1.2")
-- >      ]
cachingDnsServer :: [UnboundSection] -> [UnboundZone] -> [UnboundHost] -> Property DebianLike
cachingDnsServer sections zones hosts =
	config `hasContent` (comment : otherSections ++ serverSection)
	`onChange` restarted
  where
	comment = "# deployed with propellor, do not modify"
	serverSection = genSection (fromMaybe ("server", []) $ find ((== "server") . fst) sections)
		++ map genZone zones
		++ map (uncurry genRecord') hosts
	otherSections = foldr ((++) . genSection) [] $ filter ((/= "server") . fst) sections

genSection :: UnboundSection -> [Line]
genSection (section, settings) = sectionHeader section : map genSetting settings

genSetting :: UnboundSetting -> Line
genSetting (key, value) = "    " ++ key ++ ": " ++ value

genZone :: UnboundZone -> Line
genZone (dom, zt) = "    local-zone: \"" ++ dValue dom ++ "\" " ++ zt

genRecord' :: BindDomain -> Record -> Line
genRecord' dom r = "    local-data: \"" ++ fromMaybe "" (genRecord dom r) ++ "\""

genRecord :: BindDomain -> Record -> Maybe String
genRecord dom (Address addr) = Just $ genAddressNoTtl dom addr
genRecord dom (MX priority dest) = Just $ unwords
	[ dValue dom
	, "MX"
	, val priority
	, dValue dest
	]
genRecord dom (PTR revip) = Just $ unwords
	[ revip ++ "."
	, "PTR"
	, dValue dom
	]
genRecord dom (CNAME dest) = Just $ unwords
	[ dValue dom
	, "CNAME"
	, dValue dest
	]
genRecord dom (NS serv) = Just $ unwords
	[ dValue dom
	, "NS"
	, dValue serv
	]
genRecord dom (TXT txt) = Just $ unwords
	[ dValue dom
	, "TXT"
	, txt
	]
genRecord dom (SRV priority weight port target) = Just $ unwords
	[ dValue dom
	, "SRV"
	, val priority
	, val weight
	, val port
	, dValue target
	]
genRecord dom (SSHFP algo hash fingerprint) = Just $ unwords
	[ dValue dom
	, "SSHFP"
	, val algo
	, val hash
	, fingerprint
	]
genRecord _ (INCLUDE _) = Nothing

genAddressNoTtl :: BindDomain -> IPAddr -> String
genAddressNoTtl dom = genAddress dom Nothing

genAddress :: BindDomain -> Maybe Int -> IPAddr -> String
genAddress dom ttl addr = case addr of
	IPv4 _ -> genAddress' "A" dom ttl addr
	IPv6 _ -> genAddress' "AAAA" dom ttl addr

genAddress' :: String -> BindDomain -> Maybe Int -> IPAddr -> String
genAddress' recordtype dom ttl addr = unwords $
	[ dValue dom ]
	++ maybe [] (\ttl' -> [val ttl']) ttl ++
	[ "IN"
	, recordtype
	, val addr
	]
