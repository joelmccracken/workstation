{-# LANGUAGE TypeFamilies #-}

module Propellor.Property.Tor where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.ConfFile as ConfFile
import Utility.DataUnits

import System.Posix.Files
import Data.Char
import Data.List

type HiddenServiceName = String

type NodeName = String

-- | Sets up a tor bridge. (Not a relay or exit node.)
--
-- Uses port 443
isBridge :: Property DebianLike
isBridge = configured
	[ ("BridgeRelay", "1")
	, ("Exitpolicy", "reject *:*")
	, ("ORPort", "443")
	]
	`describe` "tor bridge"
	`requires` server

-- | Sets up a tor relay.
--
-- Uses port 443
isRelay :: Property DebianLike
isRelay = configured
	[ ("BridgeRelay", "0")
	, ("Exitpolicy", "reject *:*")
	, ("ORPort", "443")
	]
	`describe` "tor relay"
	`requires` server

-- | Makes the tor node be named, with a known private key.
--
-- This can be moved to a different IP without needing to wait to
-- accumulate trust.
named :: NodeName -> Property (HasInfo + DebianLike)
named n = configured [("Nickname", n')]
	`describe` ("tor node named " ++ n')
	`requires` torPrivKey (Context ("tor " ++ n))
  where
	n' = saneNickname n

-- | Configures tor with secret_id_key, ed25519_master_id_public_key,
-- and ed25519_master_id_secret_key from privdata.
torPrivKey :: Context -> Property (HasInfo + DebianLike)
torPrivKey context = mconcat (map go keyfiles)
	`onChange` restarted
	`requires` torPrivKeyDirExists
  where
	keyfiles = map (torPrivKeyDir </>)
		[ "secret_id_key"
		, "ed25519_master_id_public_key"
		, "ed25519_master_id_secret_key"
		]
	go f = f `File.hasPrivContent` context
		`onChange` File.ownerGroup f user (userGroup user)

torPrivKeyDirExists :: Property DebianLike
torPrivKeyDirExists = File.dirExists torPrivKeyDir
	`onChange` setperms
	`requires` installed
  where
	setperms = File.ownerGroup torPrivKeyDir user (userGroup user)
		`before` File.mode torPrivKeyDir 0O2700

torPrivKeyDir :: FilePath
torPrivKeyDir = "/var/lib/tor/keys"

-- | A tor server (bridge, relay, or exit)
-- Don't use if you just want to run tor for personal use.
server :: Property DebianLike
server = configured [("SocksPort", "0")]
	`requires` installed
	`requires` Apt.installed ["ntp"]
	`describe` "tor server"

installed :: Property DebianLike
installed = Apt.installed ["tor"]

-- | Specifies configuration settings. Any lines in the config file
-- that set other values for the specified settings will be removed,
-- while other settings are left as-is. Tor is restarted when
-- configuration is changed.
configured :: [(String, String)] -> Property DebianLike
configured settings = File.fileProperty "tor configured" go mainConfig
	`onChange` restarted
  where
	ks = map fst settings
	go ls = sort $ map toconfig $
		filter (\(k, _) -> k `notElem` ks) (map fromconfig ls)
		++ settings
	toconfig (k, v) = k ++ " " ++ v
	fromconfig = separate (== ' ')

data BwLimit
	= PerSecond String
	| PerDay String
	| PerMonth String

-- | Limit incoming and outgoing traffic to the specified
-- amount each.
--
-- For example, PerSecond "30 kibibytes" is the minimum limit
-- for a useful relay.
bandwidthRate :: BwLimit -> Property DebianLike
bandwidthRate (PerSecond s) = bandwidthRate' s 1
bandwidthRate (PerDay s) = bandwidthRate' s (24*60*60)
bandwidthRate (PerMonth s) = bandwidthRate' s (31*24*60*60)

bandwidthRate' :: String -> Integer -> Property DebianLike
bandwidthRate' s divby = case readSize dataUnits s of
	Just sz -> let v = show (sz `div` divby) ++ " bytes"
		in configured [("BandwidthRate", v)]
			`describe` ("tor BandwidthRate " ++ v)
	Nothing -> property ("unable to parse " ++ s) noChange

-- | Enables a hidden service for a given port.
--
-- If used without `hiddenServiceData`, tor will generate a new
-- private key.
hiddenService :: HiddenServiceName -> Port -> Property DebianLike
hiddenService hn port = hiddenService' hn [port]

hiddenService' :: HiddenServiceName -> [Port] -> Property DebianLike
hiddenService' hn ports = ConfFile.adjustSection
	(unwords ["hidden service", hn, "available on ports", intercalate "," (map val ports')])
	(== oniondir)
	(not . isPrefixOf "HiddenServicePort")
	(const (oniondir : onionports))
	(++ oniondir : onionports)
	mainConfig
	`onChange` restarted
  where
	oniondir = unwords ["HiddenServiceDir", varLib </> hn]
	onionports = map onionport ports'
	ports' = sort ports
	onionport port = unwords ["HiddenServicePort", val port, "127.0.0.1:" ++ val port]

-- | Same as `hiddenService` but also causes propellor to display
-- the onion address of the hidden service.
hiddenServiceAvailable :: HiddenServiceName -> Port -> Property DebianLike
hiddenServiceAvailable hn port = hiddenServiceAvailable' hn [port]

hiddenServiceAvailable' :: HiddenServiceName -> [Port] -> Property DebianLike
hiddenServiceAvailable' hn ports = hiddenServiceHostName $ hiddenService' hn ports
  where
	hiddenServiceHostName p =  adjustPropertySatisfy p $ \satisfy -> do
		r <- satisfy
		mh <- liftIO $ tryIO $ readFile (varLib </> hn </> "hostname")
		case mh of
			Right h -> infoMessage ["hidden service hostname:", h]
			Left _e -> warningMessage "hidden service hostname not available yet"
		return r

-- | Load the private key for a hidden service from the privdata.
hiddenServiceData :: IsContext c => HiddenServiceName -> c -> Property (HasInfo + DebianLike)
hiddenServiceData hn context = combineProperties desc $ props
	& installonion "hostname"
	& installonion "private_key"
  where
	desc = unwords ["hidden service data available in", varLib </> hn]
	installonion :: FilePath -> Property (HasInfo + DebianLike)
	installonion basef =
		let f = varLib </> hn </> basef
		in withPrivData (PrivFile f) context $ \getcontent ->
		property' desc $ \w -> getcontent $ \privcontent ->
			ifM (liftIO $ doesFileExist f)
				( noChange
				, ensureProperty w $ propertyList desc $ toProps
					[ property desc $ makeChange $ do
						createDirectoryIfMissing True (takeDirectory f)
						writeFileProtected f (unlines (privDataLines privcontent))
					, File.mode (takeDirectory f) $ combineModes
						[ownerReadMode, ownerWriteMode, ownerExecuteMode]
					, File.ownerGroup (takeDirectory f) user (userGroup user)
					, File.ownerGroup f user (userGroup user)
					]
				)

restarted :: Property DebianLike
restarted = Service.restarted "tor"

mainConfig :: FilePath
mainConfig = "/etc/tor/torrc"

varLib :: FilePath
varLib = "/var/lib/tor"

varRun :: FilePath
varRun = "/var/run/tor"

user :: User
user = User "debian-tor"

type NickName = String

-- | Convert String to a valid tor NickName.
saneNickname :: String -> NickName
saneNickname s
	| null n = "unnamed"
	| otherwise = n
  where
	legal c = isNumber c || isAsciiUpper c || isAsciiLower c
	n = take 19 $ filter legal s
