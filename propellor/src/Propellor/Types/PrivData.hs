module Propellor.Types.PrivData where

import Propellor.Types.OS
import Utility.PartialPrelude
import Utility.FileSystemEncoding

import Data.Maybe
import qualified Data.ByteString.Lazy as L

-- | Note that removing or changing constructors or changing types will
-- break the serialized privdata files, so don't do that!
-- It's fine to add new constructors.
data PrivDataField
	= DockerAuthentication
	| SshPubKey SshKeyType UserName -- ^ Not used anymore, but retained to avoid breaking serialization of old files
	| SshPrivKey SshKeyType UserName -- ^ For host key, use empty UserName
	| SshAuthorizedKeys UserName
	| Password UserName
	| CryptPassword UserName
	| PrivFile FilePath
	| GpgKey
	| DnsSec DnsSecKey
	deriving (Read, Show, Ord, Eq)

-- | Combines a PrivDataField with a description of how to generate
-- its value.
data PrivDataSource
	= PrivDataSourceFile PrivDataField FilePath
	| PrivDataSourceFileFromCommand PrivDataField FilePath String
	| PrivDataSource PrivDataField String

type PrivDataSourceDesc = String

class IsPrivDataSource s where
	privDataField :: s -> PrivDataField
	describePrivDataSource :: s -> Maybe PrivDataSourceDesc

instance IsPrivDataSource PrivDataField where
	privDataField = id
	describePrivDataSource _ = Nothing

instance IsPrivDataSource PrivDataSource where
	privDataField s = case s of
		PrivDataSourceFile f _ -> f
		PrivDataSourceFileFromCommand f _ _ -> f
		PrivDataSource f _ -> f
	describePrivDataSource s = Just $ case s of
		PrivDataSourceFile _ f -> "< " ++ f
		PrivDataSourceFileFromCommand _ f c ->
			"< " ++ f ++ " (created by running, for example, `" ++ c ++ "` )"
		PrivDataSource _ d -> "< (" ++ d ++ ")"

-- | A context in which a PrivDataField is used.
--
-- Often this will be a domain name. For example, 
-- Context "www.example.com" could be used for the SSL cert
-- for the web server serving that domain. Multiple hosts might
-- use that privdata.
--
-- This appears in serialized privdata files.
newtype Context = Context String
	deriving (Read, Show, Ord, Eq)

-- | A context that may vary depending on the HostName where it's used.
newtype HostContext = HostContext { mkHostContext :: HostName -> Context }

instance Show HostContext where
	show hc = show $ mkHostContext hc "<hostname>"

instance Ord HostContext where
	a <= b = show a <= show b

instance Eq HostContext where
	a == b = show a == show b

-- | Class of things that can be used as a Context.
class IsContext c where
	asContext :: HostName -> c -> Context
	asHostContext :: c -> HostContext

instance IsContext HostContext where
	asContext = flip mkHostContext
	asHostContext = id

instance IsContext Context where
	asContext _ c = c
	asHostContext = HostContext . const

-- | Use when a PrivDataField is not dependent on any paricular context.
anyContext :: Context
anyContext = Context "any"

-- | Makes a HostContext that consists just of the hostname.
hostContext :: HostContext
hostContext = HostContext Context

-- | Contains the actual private data.
--
-- Note that this may contain exta newlines at the end, or they may have
-- been stripped off, depending on how the user entered the privdata,
-- and which version of propellor stored it. Use the accessor functions
-- below to avoid newline problems.
newtype PrivData = PrivData String

-- | When PrivData is the content of a file, this is the lines thereof.
privDataLines :: PrivData -> [String]
privDataLines (PrivData s) = lines s

-- | When the PrivData is a single value, like a password, this extracts
-- it. Note that if multiple lines are present in the PrivData, only
-- the first is returned; there is never a newline in the String.
privDataVal :: PrivData -> String
privDataVal (PrivData s) = fromMaybe "" (headMaybe (lines s))

-- | Use to get ByteString out of PrivData.
privDataByteString :: PrivData -> L.ByteString
privDataByteString (PrivData s) = encodeBS s

data SshKeyType = SshRsa | SshDsa | SshEcdsa | SshEd25519
	deriving (Read, Show, Ord, Eq, Enum, Bounded)

-- | Parameter that would be passed to ssh-keygen to generate key of this type
sshKeyTypeParam :: SshKeyType -> String
sshKeyTypeParam SshRsa = "RSA"
sshKeyTypeParam SshDsa = "DSA"
sshKeyTypeParam SshEcdsa = "ECDSA"
sshKeyTypeParam SshEd25519 = "ED25519"

data DnsSecKey
	= PubZSK -- ^ DNSSEC Zone Signing Key (public)
	| PrivZSK -- ^ DNSSEC Zone Signing Key (private)
	| PubKSK -- ^ DNSSEC Key Signing Key (public)
	| PrivKSK -- ^ DNSSEC Key Signing Key (private)
	deriving (Read, Show, Ord, Eq, Bounded, Enum)
