{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Postfix where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.User as User

import qualified Data.Map as M
import Data.List
import Data.Char

installed :: Property DebianLike
installed = Apt.serviceInstalledRunning "postfix"

restarted :: Property DebianLike
restarted = Service.restarted "postfix"

reloaded :: Property DebianLike
reloaded = Service.reloaded "postfix"

-- | Configures postfix as a satellite system, which 
-- relays all mail through a relay host, which defaults to smtp.domain,
-- but can be changed by @mainCf "relayhost"@.
--
-- The smarthost may refuse to relay mail on to other domains, without
-- further configuration/keys. But this should be enough to get cron job
-- mail flowing to a place where it will be seen.
satellite :: Property DebianLike
satellite = check (not <$> mainCfIsSet "relayhost") setup
	`requires` installed
  where
	desc = "postfix satellite system"
	setup :: Property DebianLike
	setup = property' desc $ \w -> do
		hn <- asks hostName
		let (_, domain) = separate (== '.') hn
		ensureProperty w $ combineProperties desc $ props
			& Apt.reConfigure "postfix"
				[ ("postfix/main_mailer_type", "select", "Satellite system")
				, ("postfix/root_address", "string", "root")
				, ("postfix/destinations", "string", "localhost")
				, ("postfix/mailname", "string", hn)
				]
			& mainCf ("relayhost", "smtp." ++ domain)
				`onChange` reloaded

-- | Sets up a file by running a property (which the filename is passed
-- to). If the setup property makes a change, postmap will be run on the
-- file, and postfix will be reloaded.
mappedFile
	:: Combines (Property x) (Property UnixLike)
	=> FilePath
	-> (FilePath -> Property x)
	-> CombinedType (Property x) (Property UnixLike)
mappedFile f setup = setup f
	`onChange` (cmdProperty "postmap" [f] `assume` MadeChange)

-- | Run newaliases command, which should be done after changing
-- @/etc/aliases@.
newaliases :: Property UnixLike
newaliases = check ("/etc/aliases" `isNewerThan` "/etc/aliases.db")
	(cmdProperty "newaliases" [])

-- | The main config file for postfix.
mainCfFile :: FilePath
mainCfFile = "/etc/postfix/main.cf"

-- | Sets a main.cf @name=value@ pair. Does not reload postfix immediately.
mainCf :: (String, String) -> Property UnixLike
mainCf (name, value) = check notset set
	`describe` ("postfix main.cf " ++ setting)
  where
	setting = name ++ "=" ++ value
	notset = (/= Just value) <$> getMainCf name
	set = cmdProperty "postconf" ["-e", setting]

-- | Gets a main.cf setting.
getMainCf :: String -> IO (Maybe String)
getMainCf name = parse . lines <$> readProcess "postconf" [name]
  where
	parse (l:_) = Just $ 
		case separate (== '=') l of
			(_, (' ':v)) -> v
			(_, v) -> v
	parse [] = Nothing

-- | Checks if a main.cf field is set. A field that is set to
-- the empty string is considered not set.
mainCfIsSet :: String -> IO Bool
mainCfIsSet name = do
	v <- getMainCf name
	return $ v /= Nothing && v /= Just ""

-- | Parses main.cf, and removes any initial configuration lines that are
-- overridden to other values later in the file.
--
-- For example, to add some settings, removing any old settings:
--
-- > mainCf `File.containsLines`
-- >	[ "# I like bars."
-- >	, "foo = bar"
-- >	] `onChange` dedupMainCf
--
-- Note that multiline configurations that continue onto the next line
-- are not currently supported.
dedupMainCf :: Property UnixLike
dedupMainCf = File.fileProperty "postfix main.cf dedupped" dedupCf mainCfFile

dedupCf :: [String] -> [String]
dedupCf ls =
	let parsed = map parse ls
	in dedup [] (keycounts $ rights parsed) parsed
  where	
	parse l
		| "#" `isPrefixOf` l = Left l
		| "=" `isInfixOf` l = 
			let (k, v) = separate (== '=') l
			in Right ((filter (not . isSpace) k), v)
		| otherwise = Left l
	fmt k v = k ++ " =" ++ v

	keycounts = M.fromListWith (+) . map (\(k, _v) -> (k, (1 :: Integer)))

	dedup c _ [] = reverse c
	dedup c kc ((Left v):rest) = dedup (v:c) kc rest
	dedup c kc ((Right (k, v)):rest) = case M.lookup k kc of
		Just n | n > 1 -> dedup c (M.insert k (n - 1) kc) rest
		_ -> dedup (fmt k v:c) kc rest

-- | The master config file for postfix.
masterCfFile :: FilePath
masterCfFile = "/etc/postfix/master.cf"

-- | A service that can be present in the master config file.
data Service = Service
	{ serviceType :: ServiceType
	, serviceCommand :: String
	, serviceOpts :: ServiceOpts
	}
	deriving (Show, Eq)

data ServiceType 
	= InetService (Maybe HostName) ServicePort
	| UnixService FilePath PrivateService
	| FifoService FilePath PrivateService
	| PassService FilePath PrivateService
	deriving (Show, Eq)

-- Can be a port number or service name such as "smtp".
type ServicePort = String

type PrivateService = Bool

-- | Options for a service.
data ServiceOpts = ServiceOpts
	{ serviceUnprivileged :: Maybe Bool
	, serviceChroot :: Maybe Bool
	, serviceWakeupTime :: Maybe Int
	, serviceProcessLimit :: Maybe Int
	}
	deriving (Show, Eq)

defServiceOpts :: ServiceOpts
defServiceOpts = ServiceOpts
	{ serviceUnprivileged = Nothing
	, serviceChroot = Nothing
	, serviceWakeupTime = Nothing
	, serviceProcessLimit = Nothing
	}

formatServiceLine :: Service -> File.Line
formatServiceLine s = unwords $ map pad
	[ (10, case serviceType s of
		InetService (Just h) p -> h ++ ":" ++ p
		InetService Nothing p -> p
		UnixService f _ -> f
		FifoService f _ -> f
		PassService f _ -> f)
	, (6, case serviceType s of
		InetService _ _ -> "inet"
		UnixService _ _ -> "unix"
		FifoService _ _ -> "fifo"
		PassService _ _ -> "pass")
	, (8, case serviceType s of
		InetService _ _ -> bool False
		UnixService _ b -> bool b
		FifoService _ b -> bool b
		PassService _ b -> bool b)
	, (8, v bool serviceUnprivileged)
	, (8, v bool serviceChroot)
	, (8, v show serviceWakeupTime)
	, (8, v show serviceProcessLimit)
	, (0, serviceCommand s)
	]
  where
	v f sel = maybe "-" f (sel (serviceOpts s))
	bool True = "y"
	bool False = "n"
	pad (n, t) = t ++ replicate (n - 1 - length t) ' '

-- | Note that this does not handle multi-line service entries,
-- in which subsequent lines are indented. `serviceLine` does not generate
-- such entries.
parseServiceLine :: File.Line -> Maybe Service
parseServiceLine ('#':_) = Nothing
parseServiceLine (' ':_) = Nothing -- continuation of multiline entry
parseServiceLine l = Service
	<$> parsetype
	<*> parsecommand
	<*> parseopts
  where
	parsetype = do
		t <- getword 2
		case t of
			"inet" -> do
				v <- getword 1
				let (h,p) = separate (== ':') v
				if null p
					then Nothing
					else Just $ InetService
						(if null h then Nothing else Just h) p
			"unix" -> UnixService <$> getword 1 <*> parseprivate
			"fifo" -> FifoService <$> getword 1 <*> parseprivate
			"pass" -> PassService <$> getword 1 <*> parseprivate
			_ -> Nothing
	parseprivate = join . bool =<< getword 3
	
	parsecommand = case unwords (drop 7 ws) of
		"" -> Nothing
		s -> Just s

	parseopts = ServiceOpts
		<$> (bool =<< getword 4)
		<*> (bool =<< getword 5)
		<*> (int =<< getword 6)
		<*> (int =<< getword 7)

	bool "-" = Just Nothing
	bool "y" = Just (Just True)
	bool "n" = Just (Just False)
	bool _ = Nothing

	int "-" = Just Nothing
	int n = maybe Nothing (Just . Just) (readish n)

	getword n
		| nws >= n = Just (ws !! (n -1))
		| otherwise = Nothing
	ws = words l
	nws = length ws

-- | Enables a `Service` in postfix's `masterCfFile`.
service :: Service -> RevertableProperty DebianLike DebianLike
service s = (enable <!> disable)
	`describe` desc
  where
	desc = "enabled postfix service " ++ show (serviceType s)
	enable = masterCfFile `File.containsLine` (formatServiceLine s)
		`onChange` reloaded
	disable = File.fileProperty desc (filter (not . matches)) masterCfFile
		`onChange` reloaded
	matches l = case parseServiceLine l of
		Just s' | s' == s -> True
		_ -> False

-- | Installs saslauthd and configures it for postfix, authenticating
-- against PAM.
--
-- Does not configure postfix to use it; eg @smtpd_sasl_auth_enable = yes@
-- needs to be set to enable use. See
-- <https://wiki.debian.org/PostfixAndSASL>.
--
-- Password brute force attacks are possible when SASL auth is enabled.
-- It would be wise to enable fail2ban, for example:
--
-- > Fail2Ban.jailEnabled "postfix-sasl"
saslAuthdInstalled :: Property DebianLike
saslAuthdInstalled = setupdaemon
	`requires` Service.running "saslauthd"
	`requires` postfixgroup
	`requires` dirperm
	`requires` Apt.installed ["sasl2-bin"]
	`requires` smtpdconf
  where
	setupdaemon = "/etc/default/saslauthd" `File.containsLines`
		[ "START=yes" 
		, "OPTIONS=\"-c -m " ++ dir ++ "\""
		]
		`onChange` Service.restarted "saslauthd"
	smtpdconf = "/etc/postfix/sasl/smtpd.conf" `File.containsLines`
		[ "pwcheck_method: saslauthd"
		, "mech_list: PLAIN LOGIN"
		]
	dirperm = check (not <$> doesDirectoryExist dir) $ 
		cmdProperty "dpkg-statoverride"
			[ "--add", "root", "sasl", "710", dir ]
	postfixgroup = (User "postfix") `User.hasGroup` (Group "sasl")
		`onChange` restarted
	dir = "/var/spool/postfix/var/run/saslauthd"

-- | Uses `saslpasswd2` to set the password for a user in the sasldb2 file.
--
-- The password is taken from the privdata.
saslPasswdSet :: Domain -> User -> Property (HasInfo + UnixLike)
saslPasswdSet domain (User user) = go `changesFileContent` "/etc/sasldb2"
  where
	go = withPrivData src ctx $ \getpw ->
		property desc $ getpw $ \pw -> liftIO $
			withHandle StdinHandle createProcessSuccess p $ \h -> do
				hPutStrLn h (privDataVal pw)
				hClose h
				return NoChange
	desc = "sasl password for " ++ uatd
	uatd = user ++ "@" ++ domain
	ps = ["-p", "-c", "-u", domain, user]
	p = proc "saslpasswd2" ps
	ctx = Context "sasl"
	src = PrivDataSource (Password uatd) "enter password"
