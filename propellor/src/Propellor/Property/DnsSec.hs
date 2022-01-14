module Propellor.Property.DnsSec where

import Propellor.Base
import qualified Propellor.Property.File as File

-- | Puts the DNSSEC key files in place from PrivData.
--
-- signedPrimary uses this, so this property does not normally need to be
-- used directly.
keysInstalled :: Domain -> RevertableProperty (HasInfo + UnixLike) UnixLike
keysInstalled domain = setup <!> cleanup
  where
	setup = propertyList "DNSSEC keys installed" $ toProps $
		map installkey keys

	cleanup = propertyList "DNSSEC keys removed" $ toProps $
		map (File.notPresent . keyFn domain) keys

	installkey k = writer (keysrc k) (keyFn domain k) (Context domain)
	  where
		writer
			| isPublic k = File.hasPrivContentExposedFrom
			| otherwise = File.hasPrivContentFrom

	keys = [ PubZSK, PrivZSK, PubKSK, PrivKSK ]

	keysrc k = PrivDataSource (DnsSec k) $ unwords
		[ "The file with extension"
		, keyExt k
		, "created by running:"
		, if isZoneSigningKey k
			then "dnssec-keygen -a RSASHA256 -b 2048 -n ZONE " ++ domain
			else "dnssec-keygen -f KSK -a RSASHA256 -b 4096 -n ZONE " ++ domain
		]

-- | Uses dnssec-signzone to sign a domain's zone file.
--
-- signedPrimary uses this, so this property does not normally need to be
-- used directly.
zoneSigned :: Domain -> FilePath -> RevertableProperty (HasInfo + UnixLike) UnixLike
zoneSigned domain zonefile = setup <!> cleanup
  where
	setup :: Property (HasInfo + UnixLike)
	setup = check needupdate (forceZoneSigned domain zonefile)
		`requires` keysInstalled domain
	
	cleanup :: Property UnixLike
	cleanup = File.notPresent (signedZoneFile zonefile)
		`before` File.notPresent dssetfile
		`before` revert (keysInstalled domain)
	
	dssetfile = dir </> "-" ++ domain ++ "."
	dir = takeDirectory zonefile

	-- Need to update the signed zone file if the zone file or
	-- any of the keys have a newer timestamp.
	needupdate = do
		v <- catchMaybeIO $ getModificationTime (signedZoneFile zonefile)
		case v of
			Nothing -> return True
			Just t1 -> anyM (newerthan t1) $
				zonefile : map (keyFn domain) [minBound..maxBound]

	newerthan t1 f = do
		t2 <- getModificationTime f
		return (t2 >= t1)

forceZoneSigned :: Domain -> FilePath -> Property UnixLike
forceZoneSigned domain zonefile = property ("zone signed for " ++ domain) $ liftIO $ do
	salt <- take 16 <$> saltSha1
 	let p = proc "dnssec-signzone"
		[ "-A"
		, "-3", salt
		-- The serial number needs to be increased each time the
		-- zone is resigned, even if there are no other changes,
		-- so that it will propagate to secondaries. So, use the
		-- unixtime serial format.
		, "-N", "unixtime"
		, "-o", domain
		, zonefile
		-- the ordering of these key files does not matter
		, keyFn domain PubZSK  
		, keyFn domain PubKSK
		]
	-- Run in the same directory as the zonefile, so it will 
	-- write the dsset file there.
	(_, _, _, h) <- createProcess $ 
		p { cwd = Just (takeDirectory zonefile) }
	ifM (checkSuccessProcess h)
		( return MadeChange
		, return FailedChange
		)

saltSha1 :: IO String
saltSha1 = readProcess "sh"
	[ "-c"
	, "head -c 1024 /dev/urandom | sha1sum | cut -d ' ' -f 1"
	]

-- | The file used for a given key.
keyFn :: Domain -> DnsSecKey -> FilePath
keyFn domain k =  "/etc/bind/propellor/dnssec" </> concat
	[ "K" ++ domain ++ "."
	, if isZoneSigningKey k then "ZSK" else "KSK"
	, keyExt k
	]

-- | These are the extensions that dnssec-keygen looks for.
keyExt :: DnsSecKey -> String
keyExt k
	| isPublic k = ".key"
	| otherwise = ".private"

isPublic :: DnsSecKey -> Bool
isPublic k = k `elem` [PubZSK, PubKSK]

isZoneSigningKey :: DnsSecKey -> Bool
isZoneSigningKey k = k `elem` [PubZSK, PrivZSK]

-- | dnssec-signzone makes a .signed file
signedZoneFile :: FilePath -> FilePath
signedZoneFile zonefile = zonefile ++ ".signed"
