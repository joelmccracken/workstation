module Propellor.Property.Dns (
	module Propellor.Types.Dns,
	primary,
	signedPrimary,
	secondary,
	secondaryFor,
	mkSOA,
	writeZoneFile,
	nextSerialNumber,
	adjustSerialNumber,
	serialNumberOffset,
	WarningMessage,
	genZone,
) where

import Propellor.Base
import Propellor.Types.Dns
import Propellor.Types.Info
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Service as Service
import Propellor.Property.Scheduled
import Propellor.Property.DnsSec
import Utility.Applicative

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.Split as Split (chunksOf)
import Data.List

-- | Primary dns server for a domain, using bind.
--
-- Currently, this only configures bind to serve forward DNS, not reverse DNS.
--
-- Most of the content of the zone file is configured by setting properties
-- of hosts. For example,
--
-- > host "foo.example.com"
-- >   & ipv4 "192.168.1.1"
-- >   & alias "mail.exmaple.com"
--
-- Will cause that hostmame and its alias to appear in the zone file,
-- with the configured IP address.
--
-- Also, if a host has a ssh public key configured, a SSHFP record will
-- be automatically generated for it.
--
-- The [(BindDomain, Record)] list can be used for additional records
-- that cannot be configured elsewhere. This often includes NS records,
-- TXT records and perhaps CNAMEs pointing at hosts that propellor does
-- not control.
--
-- The primary server is configured to only allow zone transfers to
-- secondary dns servers. These are determined in two ways:
--
-- 1. By looking at the properties of other hosts, to find hosts that
-- are configured as the secondary dns server.
--
-- 2. By looking for NS Records in the passed list of records.
--
-- In either case, the secondary dns server Host should have an ipv4 and/or
-- ipv6 property defined.
primary :: [Host] -> Domain -> SOA -> [(BindDomain, Record)] -> RevertableProperty (HasInfo + DebianLike) DebianLike
primary hosts domain soa rs = setup <!> cleanup
  where
	setup = setupPrimary zonefile id hosts domain soa rs
		`onChange` Service.reloaded "bind9"
	cleanup = cleanupPrimary zonefile domain
		`onChange` Service.reloaded "bind9"

	zonefile = "/etc/bind/propellor/db." ++ domain

setupPrimary :: FilePath -> (FilePath -> FilePath) -> [Host] -> Domain -> SOA -> [(BindDomain, Record)] -> Property (HasInfo + DebianLike)
setupPrimary zonefile mknamedconffile hosts domain soa rs =
	withwarnings baseprop
		`requires` servingZones
  where
	hostmap = hostMap hosts
	-- Known hosts with hostname located in the domain.
	indomain = M.elems $ M.filterWithKey (\hn _ -> inDomain domain $ AbsDomain $ hn) hostmap

	(partialzone, zonewarnings) = genZone indomain hostmap domain soa
	baseprop = primaryprop
		`setInfoProperty` (toInfo (addNamedConf conf))
	primaryprop :: Property DebianLike
	primaryprop = property ("dns primary for " ++ domain) $ do
		sshfps <- concat <$> mapM (genSSHFP domain) (M.elems hostmap)
		let zone = partialzone
			{ zHosts = zHosts partialzone ++ rs ++ sshfps }
		ifM (liftIO $ needupdate zone)
			( makeChange $ writeZoneFile zone zonefile
			, noChange
			)
	withwarnings p = adjustPropertySatisfy p $ \a -> do
		mapM_ warningMessage $ zonewarnings ++ secondarywarnings
		a
	conf = NamedConf
		{ confDomain = domain
		, confDnsServerType = Master
		, confFile = mknamedconffile zonefile
		, confMasters = []
		, confAllowTransfer = nub $
			concatMap (`hostAddresses` hosts) $
				secondaries ++ nssecondaries
		, confLines = []
		}
	secondaries = otherServers Secondary hosts domain
	secondarywarnings = map (\h -> "No IP address defined for DNS seconary " ++ h) $
		filter (\h -> null (hostAddresses h hosts)) secondaries
	nssecondaries = mapMaybe (domainHostName <=< getNS) rootRecords
	rootRecords = map snd $
		filter (\(d, _r) -> d == RootDomain || d == AbsDomain domain) rs
	needupdate zone = do
		v <- readZonePropellorFile zonefile
		return $ case v of
			Nothing -> True
			Just oldzone ->
				-- compare everything except serial
				let oldserial = sSerial (zSOA oldzone)
				    z = zone { zSOA = (zSOA zone) { sSerial = oldserial } }
				in z /= oldzone || oldserial < sSerial (zSOA zone)


cleanupPrimary :: FilePath -> Domain -> Property DebianLike
cleanupPrimary zonefile domain = check (doesFileExist zonefile) $
	go `requires` namedConfWritten
  where
	desc = "removed dns primary for " ++ domain
	go :: Property DebianLike
	go = property desc (makeChange $ removeZoneFile zonefile)

-- | Primary dns server for a domain, secured with DNSSEC.
--
-- This is like `primary`, except the resulting zone
-- file is signed.
-- The Zone Signing Key (ZSK) and Key Signing Key (KSK)
-- used in signing it are taken from the PrivData.
--
-- As a side effect of signing the zone, a
-- </var/cache/bind/dsset-domain.>
-- file will be created. This file contains the DS records
-- which need to be communicated to your domain registrar
-- to make DNSSEC be used for your domain. Doing so is outside
-- the scope of propellor (currently). See for example the tutorial
-- <https://www.digitalocean.com/community/tutorials/how-to-setup-dnssec-on-an-authoritative-bind-dns-server--2>
--
-- The 'Recurrance' controls how frequently the signature
-- should be regenerated, using a new random salt, to prevent
-- zone walking attacks. `Weekly Nothing` is a reasonable choice.
--
-- To transition from 'primary' to 'signedPrimary', you can revert
-- the 'primary' property, and add this property.
--
-- Note that DNSSEC zone files use a serial number based on the unix epoch.
-- This is different from the serial number used by 'primary', so if you
-- want to later disable DNSSEC you will need to adjust the serial number
-- passed to mkSOA to ensure it is larger.
signedPrimary :: Recurrance -> [Host] -> Domain -> SOA -> [(BindDomain, Record)] -> RevertableProperty (HasInfo + DebianLike) DebianLike
signedPrimary recurrance hosts domain soa rs = setup <!> cleanup
  where
	setup = combineProperties ("dns primary for " ++ domain ++ " (signed)")
		(props
			& setupPrimary zonefile signedZoneFile hosts domain soa rs'
			& zoneSigned domain zonefile
			& forceZoneSigned domain zonefile `period` recurrance
		)
		`onChange` Service.reloaded "bind9"

	cleanup = cleanupPrimary zonefile domain
		`onChange` revert (zoneSigned domain zonefile)
		`onChange` Service.reloaded "bind9"

	-- Include the public keys into the zone file.
	rs' = include PubKSK : include PubZSK : rs
	include k = (RootDomain, INCLUDE (keyFn domain k))

	-- Put DNSSEC zone files in a different directory than is used for
	-- the regular ones. This allows 'primary' to be reverted and
	-- 'signedPrimary' enabled, without the reverted property stomping
	-- on the new one's settings.
	zonefile = "/etc/bind/propellor/dnssec/db." ++ domain

-- | Secondary dns server for a domain.
--
-- The primary server is determined by looking at the properties of other
-- hosts to find which one is configured as the primary.
--
-- Note that if a host is declared to be a primary and a secondary dns
-- server for the same domain, the primary server config always wins.
secondary :: [Host] -> Domain -> RevertableProperty (HasInfo + DebianLike) DebianLike
secondary hosts domain = secondaryFor (otherServers Master hosts domain) hosts domain

-- | This variant is useful if the primary server does not have its DNS
-- configured via propellor.
secondaryFor :: [HostName] -> [Host] -> Domain -> RevertableProperty (HasInfo + DebianLike) DebianLike
secondaryFor masters hosts domain = setup <!> cleanup
  where
	setup = pureInfoProperty desc (addNamedConf conf)
		`requires` servingZones
	cleanup = namedConfWritten

	desc = "dns secondary for " ++ domain
	conf = NamedConf
		{ confDomain = domain
		, confDnsServerType = Secondary
		, confFile = "db." ++ domain
		, confMasters = concatMap (`hostAddresses` hosts) masters
		, confAllowTransfer = []
		, confLines = []
		}

otherServers :: DnsServerType -> [Host] -> Domain -> [HostName]
otherServers wantedtype hosts domain =
	M.keys $ M.filter wanted $ hostMap hosts
  where
	wanted h = case M.lookup domain (fromNamedConfMap $ fromInfo $ hostInfo h) of
		Nothing -> False
		Just conf -> confDnsServerType conf == wantedtype
			&& confDomain conf == domain

-- | Rewrites the whole named.conf.local file to serve the zones
-- configured by `primary` and `secondary`, and ensures that bind9 is
-- running.
servingZones :: Property DebianLike
servingZones = namedConfWritten
	`onChange` Service.reloaded "bind9"
	`requires` Apt.serviceInstalledRunning "bind9"

namedConfWritten :: Property DebianLike
namedConfWritten = property' "named.conf configured" $ \w -> do
	zs <- getNamedConf
	ensureProperty w $
		hasContent namedConfFile $
			concatMap confStanza $ M.elems zs

confStanza :: NamedConf -> [Line]
confStanza c =
	[ "// automatically generated by propellor"
	, "zone \"" ++ confDomain c ++ "\" {"
	, cfgline "type" (if confDnsServerType c == Master then "master" else "slave")
	, cfgline "file" ("\"" ++ confFile c ++ "\"")
	] ++
	mastersblock ++
	allowtransferblock ++
	(map (\l -> "\t" ++ l ++ ";") (confLines c)) ++
	[ "};"
	, ""
	]
  where
	cfgline f v = "\t" ++ f ++ " " ++ v ++ ";"
	ipblock name l =
		[ "\t" ++ name ++ " {" ] ++
		(map (\ip -> "\t\t" ++ val ip ++ ";") l) ++
		[ "\t};" ]
	mastersblock
		| null (confMasters c) = []
		| otherwise = ipblock "masters" (confMasters c)
	-- an empty block prohibits any transfers
	allowtransferblock = ipblock "allow-transfer" (confAllowTransfer c)

namedConfFile :: FilePath
namedConfFile = "/etc/bind/named.conf.local"

-- | Generates a SOA with some fairly sane numbers in it.
--
-- The Domain is the domain to use in the SOA record. Typically
-- something like ns1.example.com. So, not the domain that this is the SOA
-- record for.
--
-- The SerialNumber can be whatever serial number was used by the domain
-- before propellor started managing it. Or 0 if the domain has only ever
-- been managed by propellor.
--
-- You do not need to increment the SerialNumber when making changes!
-- Propellor will automatically add the number of commits in the git
-- repository to the SerialNumber.
mkSOA :: Domain -> SerialNumber -> SOA
mkSOA d sn = SOA
	{ sDomain = AbsDomain d
	, sSerial = sn
	, sRefresh = hours 4
	, sRetry = hours 1
	, sExpire = 2419200 -- 4 weeks
	, sNegativeCacheTTL = hours 8
	}
  where
	hours n = n * 60 * 60

dValue :: BindDomain -> String
dValue (RelDomain d) = d
dValue (AbsDomain d) = d ++ "."
dValue (RootDomain) = "@"

rField :: Record -> Maybe String
rField (Address (IPv4 _)) = Just "A"
rField (Address (IPv6 _)) = Just "AAAA"
rField (CNAME _) = Just "CNAME"
rField (MX _ _) = Just "MX"
rField (NS _) = Just "NS"
rField (TXT _) = Just "TXT"
rField (SRV _ _ _ _) = Just "SRV"
rField (SSHFP _ _ _) = Just "SSHFP"
rField (INCLUDE _) = Just "$INCLUDE"
rField (PTR _) = Nothing

rValue :: Record -> Maybe String
rValue (Address (IPv4 addr)) = Just addr
rValue (Address (IPv6 addr)) = Just addr
rValue (CNAME d) = Just $ dValue d
rValue (MX pri d) = Just $ val pri ++ " " ++ dValue d
rValue (NS d) = Just $ dValue d
rValue (SRV priority weight port target) = Just $ unwords
	[ val priority
	, val weight
	, val port
	, dValue target
	]
rValue (SSHFP x y s) = Just $ unwords
	[ val x
	, val y
	, s
	]
rValue (INCLUDE f) = Just f
rValue (TXT s) = Just $ zoneFileString s
rValue (PTR _) = Nothing

-- Bind has a limit on the length of a string in its zone file,
-- but a string can be split into sections that are glued together
-- inside parens to configure a longer value.
--
-- This adds quotes around each substring.
zoneFileString :: String -> String
zoneFileString s = concat
	[ [op, w]
	, (intercalate "\n\t" $
		map (\x -> [q] ++ filter (/= q) x ++ [q]) $
		Split.chunksOf 255 s)
	, [w, cp]
	]
  where
	op = '('
	cp = ')'
	w = ' '
	q = '"'

-- | Adjusts the serial number of the zone to always be larger
-- than the serial number in the Zone record,
-- and always be larger than the passed SerialNumber.
nextSerialNumber :: Zone -> SerialNumber -> Zone
nextSerialNumber z serial = adjustSerialNumber z $ \sn -> succ $ max sn serial

adjustSerialNumber :: Zone -> (SerialNumber -> SerialNumber) -> Zone
adjustSerialNumber (Zone d soa l) f = Zone d soa' l
  where
	soa' = soa { sSerial = f (sSerial soa) }

-- | Count the number of git commits made to the current branch.
serialNumberOffset :: IO SerialNumber
serialNumberOffset = fromIntegral . length . lines
	<$> readProcess "git" ["log", "--pretty=%H"]

-- | Write a Zone out to a to a file.
--
-- The serial number in the Zone automatically has the serialNumberOffset
-- added to it. Also, just in case, the old serial number used in the zone
-- file is checked, and if it is somehow larger, its succ is used.
writeZoneFile :: Zone -> FilePath -> IO ()
writeZoneFile z f = do
	oldserial <- oldZoneFileSerialNumber f
	offset <- serialNumberOffset
	let z' = nextSerialNumber
		(adjustSerialNumber z (+ offset))
		oldserial
	createDirectoryIfMissing True (takeDirectory f)
	writeFile f (genZoneFile z')
	writeZonePropellorFile f z'

removeZoneFile :: FilePath -> IO ()
removeZoneFile f = do
	nukeFile f
	nukeFile (zonePropellorFile f)

-- | Next to the zone file, is a ".propellor" file, which contains
-- the serialized Zone. This saves the bother of parsing
-- the horrible bind zone file format.
zonePropellorFile :: FilePath -> FilePath
zonePropellorFile f = f ++ ".propellor"

oldZoneFileSerialNumber :: FilePath -> IO SerialNumber
oldZoneFileSerialNumber = maybe 0 (sSerial . zSOA) <$$> readZonePropellorFile

writeZonePropellorFile :: FilePath -> Zone -> IO ()
writeZonePropellorFile f z = writeFile (zonePropellorFile f) (show z)

readZonePropellorFile :: FilePath -> IO (Maybe Zone)
readZonePropellorFile f = catchDefaultIO Nothing $
	readish <$> readFileStrict (zonePropellorFile f)

-- | Generating a zone file.
genZoneFile :: Zone -> String
genZoneFile (Zone zdomain soa rs) = unlines $
	header : genSOA soa ++ mapMaybe (genRecord zdomain) rs
  where
	header = com $ "BIND zone file for " ++ zdomain ++ ". Generated by propellor, do not edit."

genRecord :: Domain -> (BindDomain, Record) -> Maybe String
genRecord zdomain (domain, record) = case (rField record, rValue record) of
	(Nothing, _) -> Nothing
	(_, Nothing) -> Nothing
	(Just rfield, Just rvalue) -> Just $ intercalate "\t" $ case record of
		INCLUDE _ -> [ rfield, rvalue ]
		_ ->
			[ domainHost zdomain domain
			, "IN"
			, rfield
			, rvalue
			]

genSOA :: SOA -> [String]
genSOA soa =
	-- "@ IN SOA ns1.example.com. root ("
	[ intercalate "\t"
		[ dValue RootDomain
		, "IN"
		, "SOA"
		, dValue (sDomain soa)
		, "root"
		, "("
		]
	, headerline sSerial "Serial"
	, headerline sRefresh "Refresh"
	, headerline sRetry "Retry"
	, headerline sExpire "Expire"
	, headerline sNegativeCacheTTL "Negative Cache TTL"
	, inheader ")"
	]
  where
	headerline r comment = inheader $ show (r soa) ++ "\t\t" ++ com comment
	inheader l = "\t\t\t" ++ l

-- | Comment line in a zone file.
com :: String -> String
com s = "; " ++ s

type WarningMessage = String

-- | Generates a Zone for a particular Domain from the DNS properies of all
-- hosts that propellor knows about that are in that Domain.
--
-- Does not include SSHFP records.
genZone :: [Host] -> M.Map HostName Host -> Domain -> SOA -> (Zone, [WarningMessage])
genZone inzdomain hostmap zdomain soa =
	let (warnings, zhosts) = partitionEithers $ concatMap concat
		[ map hostips inzdomain
		, map hostrecords inzdomain
		, map addcnames (M.elems hostmap)
		]
	in (Zone zdomain soa (simplify zhosts), warnings)
  where
	-- Each host with a hostname located in the zdomain
	-- should have 1 or more IPAddrs in its Info.
	--
	-- If a host lacks any IPAddr, it's probably a misconfiguration,
	-- so warn.
	hostips :: Host -> [Either WarningMessage (BindDomain, Record)]
	hostips h
		| null l = [Left $ "no IP address defined for host " ++ hostName h]
		| otherwise = map Right l
	  where
		info = hostInfo h
		l = zip (repeat $ AbsDomain $ hostName h)
			(map Address $ getAddresses info)

	-- Any host, whether its hostname is in the zdomain or not,
	-- may have cnames which are in the zdomain. The cname may even be
	-- the same as the root of the zdomain, which is a nice way to
	-- specify IP addresses for a SOA record.
	--
	-- Add Records for those.. But not actually, usually, cnames!
	-- Why not? Well, using cnames doesn't allow doing some things,
	-- including MX and round robin DNS, and certianly CNAMES
	-- shouldn't be used in SOA records.
	--
	-- We typically know the host's IPAddrs anyway.
	-- So we can just use the IPAddrs.
	addcnames :: Host -> [Either WarningMessage (BindDomain, Record)]
	addcnames h = concatMap gen $ filter (inDomain zdomain) $
		mapMaybe getCNAME $ S.toList $ getDnsInfo info
	  where
		info = hostInfo h
		gen c = case getAddresses info of
			[] -> [ret (CNAME c)]
			l -> map (ret . Address) l
		  where
			ret record = Right (c, record)

	-- Adds any other DNS records for a host located in the zdomain.
	hostrecords :: Host -> [Either WarningMessage (BindDomain, Record)]
	hostrecords h = map Right l
	  where
		info = hostInfo h
		l = zip (repeat $ AbsDomain $ hostName h)
			(S.toList $ S.filter (\r -> isNothing (getIPAddr r) && isNothing (getCNAME r)) (getDnsInfo info))

	-- Simplifies the list of hosts. Remove duplicate entries.
	-- Also, filter out any CHAMES where the same domain has an
	-- IP address, since that's not legal.
	simplify :: [(BindDomain, Record)] -> [(BindDomain, Record)]
	simplify l = nub $ filter (not . dupcname ) l
	  where
		dupcname (d, CNAME _) | any (matchingaddr d) l = True
		dupcname _ = False
		matchingaddr d (d', (Address _)) | d == d' = True
		matchingaddr _ _ = False

inDomain :: Domain -> BindDomain -> Bool
inDomain domain (AbsDomain d) = domain == d || ('.':domain) `isSuffixOf` d
inDomain _ _ = False -- can't tell, so assume not

-- | Gets the hostname of the second domain, relative to the first domain,
-- suitable for using in a zone file.
domainHost :: Domain -> BindDomain -> String
domainHost _ (RelDomain d) = d
domainHost _ RootDomain = "@"
domainHost base (AbsDomain d)
	| dotbase `isSuffixOf` d = take (length d - length dotbase) d
	| base == d = "@"
	| otherwise = d
  where
	dotbase = '.':base

addNamedConf :: NamedConf -> NamedConfMap
addNamedConf conf = NamedConfMap (M.singleton domain conf)
  where
	domain = confDomain conf

getNamedConf :: Propellor (M.Map Domain NamedConf)
getNamedConf = asks $ fromNamedConfMap . fromInfo . hostInfo

-- | Generates SSHFP records for hosts in the domain (or with CNAMES
-- in the domain) that have configured ssh public keys.
--
-- This is done using ssh-keygen, so sadly needs IO.
genSSHFP :: Domain -> Host -> Propellor [(BindDomain, Record)]
genSSHFP domain h = concatMap mk . concat <$> (gen =<< get)
  where
	get = fromHost [h] hostname Ssh.getHostPubKey
	gen = liftIO . mapM genSSHFP' . M.elems . fromMaybe M.empty
	mk r = mapMaybe (\d -> if inDomain domain d then Just (d, r) else Nothing)
		(AbsDomain hostname : cnames)
	cnames = mapMaybe getCNAME $ S.toList $ getDnsInfo info
	hostname = hostName h
	info = hostInfo h

genSSHFP' :: String -> IO [Record]
genSSHFP' pubkey = withTmpFile "sshfp" $ \tmp tmph -> do
		hPutStrLn tmph pubkey
		hClose tmph
		s <- catchDefaultIO "" $
			readProcess "ssh-keygen" ["-r", "dummy", "-f", tmp]
		return $ mapMaybe (parse . words) $ lines s
  where
	parse ("dummy":"IN":"SSHFP":x:y:s:[]) = do
		x' <- readish x
		y' <- readish y
		return $ SSHFP x' y' s
	parse _ = Nothing
