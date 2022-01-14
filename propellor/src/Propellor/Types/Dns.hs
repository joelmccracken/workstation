{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Propellor.Types.Dns where

import Propellor.Types.OS (HostName)
import Propellor.Types.Empty
import Propellor.Types.Info
import Propellor.Types.ConfigurableValue
import Utility.Split

import Data.Word
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Semigroup as Sem
import Data.List
import Data.Monoid
import Prelude

type Domain = String

data IPAddr = IPv4 String | IPv6 String
	deriving (Read, Show, Eq, Ord)

instance ConfigurableValue IPAddr where
	val (IPv4 addr) = addr
	val (IPv6 addr) = addr

newtype AliasesInfo = AliasesInfo (S.Set HostName)
	deriving (Show, Eq, Ord, Sem.Semigroup, Monoid, Typeable)

instance IsInfo AliasesInfo where
	propagateInfo _ = PropagateInfo False

toAliasesInfo :: [HostName] -> AliasesInfo
toAliasesInfo l = AliasesInfo (S.fromList l)

fromAliasesInfo :: AliasesInfo -> [HostName]
fromAliasesInfo (AliasesInfo s) = S.toList s

-- | Use this for DNS Info that should propagate from a container to a
-- host. For example, this can be used for CNAME to make aliases
-- of the containers in the host be reflected in the DNS.
newtype DnsInfoPropagated = DnsInfoPropagated
	{ fromDnsInfoPropagated :: S.Set Record }
	deriving (Show, Eq, Ord, Sem.Semigroup, Monoid, Typeable)

toDnsInfoPropagated :: S.Set Record -> DnsInfoPropagated
toDnsInfoPropagated = DnsInfoPropagated

instance IsInfo DnsInfoPropagated where
	propagateInfo _ = PropagateInfo True

-- | Use this for DNS Info that should not propagate from a container to a
-- host. For example, an IP address of a container should not influence
-- the host.
newtype DnsInfoUnpropagated = DnsInfoUnpropagated
	{ fromDnsInfoUnpropagated :: S.Set Record }
	deriving (Show, Eq, Ord, Sem.Semigroup, Monoid, Typeable)

toDnsInfoUnpropagated :: S.Set Record -> DnsInfoUnpropagated
toDnsInfoUnpropagated = DnsInfoUnpropagated

-- | Get all DNS Info.
getDnsInfo :: Info -> S.Set Record
getDnsInfo i = fromDnsInfoUnpropagated (fromInfo i)
	`S.union` fromDnsInfoPropagated (fromInfo i)

instance IsInfo DnsInfoUnpropagated where
	propagateInfo _ = PropagateInfo False

-- | Represents a bind 9 named.conf file.
data NamedConf = NamedConf
	{ confDomain :: Domain
	, confDnsServerType :: DnsServerType
	, confFile :: FilePath
	, confMasters :: [IPAddr]
	, confAllowTransfer :: [IPAddr]
	, confLines :: [String]
	}
	deriving (Show, Eq, Ord)

data DnsServerType = Master | Secondary
	deriving (Show, Eq, Ord)

-- | Represents a bind 9 zone file.
data Zone = Zone
	{ zDomain :: Domain
	, zSOA :: SOA
	, zHosts :: [(BindDomain, Record)]
	}
	deriving (Read, Show, Eq)

-- | Every domain has a SOA record, which is big and complicated.
data SOA = SOA
	{ sDomain :: BindDomain
	-- ^ Typically ns1.your.domain
	, sSerial :: SerialNumber
	-- ^ The most important parameter is the serial number,
	-- which must increase after each change.
	, sRefresh :: Integer
	, sRetry :: Integer
	, sExpire :: Integer
	, sNegativeCacheTTL :: Integer
	}
	deriving (Read, Show, Eq)

-- | Types of DNS records.
--
-- This is not a complete list, more can be added.
data Record
	= Address IPAddr
	| CNAME BindDomain
	| MX Int BindDomain
	| NS BindDomain
	| TXT String
	| SRV Word16 Word16 Word16 BindDomain
	| SSHFP Int Int String
	| INCLUDE FilePath
	| PTR ReverseIP
	deriving (Read, Show, Eq, Ord, Typeable)

-- | An in-addr.arpa record corresponding to an IPAddr.
type ReverseIP = String

reverseIP :: IPAddr -> ReverseIP
reverseIP (IPv4 addr) = intercalate "." (reverse $ splitc '.' addr) ++ ".in-addr.arpa"
reverseIP addr@(IPv6 _) = reverse (intersperse '.' $ replace ":" "" $ val $ canonicalIP addr) ++ ".ip6.arpa"

-- | Converts an IP address (particularly IPv6) to canonical, fully
-- expanded form.
canonicalIP :: IPAddr -> IPAddr
canonicalIP (IPv4 addr) = IPv4 addr
canonicalIP (IPv6 addr) = IPv6 $ intercalate ":" $ map canonicalGroup $ splitc ':' $ replaceImplicitGroups addr
  where
	canonicalGroup g
		| l <= 4    = replicate (4 - l) '0' ++ g
		| otherwise = error $ "IPv6 group " ++ g ++ "as more than 4 hex digits"
	  where
		l = length g
	emptyGroups n = iterate (++ ":") "" !! n
	numberOfImplicitGroups a = 8 - length (splitc ':' $ replace "::" "" a)
	replaceImplicitGroups a = concat $ aux $ split "::" a
	  where
		aux [] = []
		aux (x : xs) = x : emptyGroups (numberOfImplicitGroups a) : xs

getIPAddr :: Record -> Maybe IPAddr
getIPAddr (Address addr) = Just addr
getIPAddr _ = Nothing

getCNAME :: Record -> Maybe BindDomain
getCNAME (CNAME d) = Just d
getCNAME _ = Nothing

getNS :: Record -> Maybe BindDomain
getNS (NS d) = Just d
getNS _ = Nothing

-- | Bind serial numbers are unsigned, 32 bit integers.
type SerialNumber = Word32

-- | Domains in the zone file must end with a period if they are absolute.
--
-- Let's use a type to keep absolute domains straight from relative
-- domains.
--
-- The RootDomain refers to the top level of the domain, so can be used
-- to add nameservers, MX's, etc to a domain.
data BindDomain = RelDomain Domain | AbsDomain Domain | RootDomain
	deriving (Read, Show, Eq, Ord)

domainHostName :: BindDomain -> Maybe HostName
domainHostName (RelDomain d) = Just d
domainHostName (AbsDomain d) = Just d
domainHostName RootDomain = Nothing

newtype NamedConfMap = NamedConfMap (M.Map Domain NamedConf)
	deriving (Eq, Ord, Show, Typeable)

instance IsInfo NamedConfMap where
	propagateInfo _ = PropagateInfo False

-- | Adding a Master NamedConf stanza for a particular domain always
-- overrides an existing Secondary stanza for that domain, while a
-- Secondary stanza is only added when there is no existing Master stanza.
instance Sem.Semigroup NamedConfMap where
	NamedConfMap old <> NamedConfMap new = NamedConfMap $
		M.unionWith combiner new old
	  where
		combiner n o = case (confDnsServerType n, confDnsServerType o) of
			(Secondary, Master) -> o
			_  -> n

instance Monoid NamedConfMap where
	mempty = NamedConfMap M.empty
	mappend = (Sem.<>)

instance Empty NamedConfMap where
	isEmpty (NamedConfMap m) = isEmpty m

fromNamedConfMap :: NamedConfMap -> M.Map Domain NamedConf
fromNamedConfMap (NamedConfMap m) = m
