{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}

module Propellor.Property.Ssh (
	installed,
	restarted,
	PubKeyText,
	SshKeyType(..),
	-- * Daemon configuration
	sshdConfig,
	ConfigKeyword,
	setSshdConfigBool,
	setSshdConfig,
	RootLogin(..),
	permitRootLogin,
	passwordAuthentication,
	noPasswords,
	listenPort,
	-- * Host keys
	randomHostKeys,
	hostKeys,
	hostKey,
	hostPubKey,
	getHostPubKey,
	-- * User keys and configuration
	userKeys,
	userKeyAt,
	knownHost,
	unknownHost,
	authorizedKeysFrom,
	unauthorizedKeysFrom,
	authorizedKeys,
	authorizedKey,
	hasAuthorizedKeys,
	getUserPubKeys,
) where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User
import Propellor.Types.Info

import System.PosixCompat
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Semigroup as Sem
import Data.List

installed :: Property UnixLike
installed = "ssh installed" ==> (aptinstall `pickOS` unsupportedOS)
  where
	aptinstall :: Property DebianLike
	aptinstall = Apt.installed ["ssh"]

restarted :: Property DebianLike
restarted = Service.restarted "ssh"

sshBool :: Bool -> String
sshBool True = "yes"
sshBool False = "no"

sshdConfig :: FilePath
sshdConfig = "/etc/ssh/sshd_config"

type ConfigKeyword = String

setSshdConfigBool :: ConfigKeyword -> Bool -> Property DebianLike
setSshdConfigBool setting allowed = setSshdConfig setting (sshBool allowed)

setSshdConfig :: ConfigKeyword -> String -> Property DebianLike
setSshdConfig setting v = File.fileProperty desc f sshdConfig
	`onChange` restarted
  where
	desc = unwords [ "ssh config:", setting, v ]
	cfgline = setting ++ " " ++ v
	wantedline s
		| s == cfgline = True
		| (setting ++ " ") `isPrefixOf` s = False
		| otherwise = True
	f ls
		| cfgline `elem` ls = filter wantedline ls
		| otherwise = filter wantedline ls ++ [cfgline]

data RootLogin
	= RootLogin Bool  -- ^ allow or prevent root login
	| WithoutPassword -- ^ disable password authentication for root, while allowing other authentication methods
	| ForcedCommandsOnly -- ^ allow root login with public-key authentication, but only if a forced command has been specified for the public key

permitRootLogin :: RootLogin -> Property DebianLike
permitRootLogin (RootLogin b) = setSshdConfigBool "PermitRootLogin" b
permitRootLogin WithoutPassword = setSshdConfig "PermitRootLogin" "without-password"
permitRootLogin ForcedCommandsOnly = setSshdConfig "PermitRootLogin" "forced-commands-only"

passwordAuthentication :: Bool -> Property DebianLike
passwordAuthentication = setSshdConfigBool "PasswordAuthentication"

-- | Configure ssh to not allow password logins.
--
-- To prevent lock-out, this is done only once root's
-- authorized_keys is in place.
noPasswords :: Property DebianLike
noPasswords = check (hasAuthorizedKeys (User "root")) $
	passwordAuthentication False

dotDir :: User -> IO FilePath
dotDir user = do
	h <- homedir user
	return $ h </> ".ssh"

dotFile :: FilePath -> User -> IO FilePath
dotFile f user = do
	d <- dotDir user
	return $ d </> f

-- | Makes the ssh server listen on a given port, in addition to any other
-- ports it is configured to listen on.
--
-- Revert to prevent it listening on a particular port.
listenPort :: Port -> RevertableProperty DebianLike DebianLike
listenPort port = enable <!> disable
  where
	portline = "Port " ++ val port
	enable = sshdConfig `File.containsLine` portline
		`describe` ("ssh listening on " ++ portline)
		`onChange` restarted
	disable = sshdConfig `File.lacksLine` portline
		`describe` ("ssh not listening on " ++ portline)
		`onChange` restarted

hasAuthorizedKeys :: User -> IO Bool
hasAuthorizedKeys = go <=< dotFile "authorized_keys"
  where
	go f = not . null <$> catchDefaultIO "" (readFile f)

-- | Blows away existing host keys and make new ones.
-- Useful for systems installed from an image that might reuse host keys.
-- A flag file is used to only ever do this once.
randomHostKeys :: Property DebianLike
randomHostKeys = flagFile prop "/etc/ssh/.unique_host_keys"
	`onChange` restarted
  where
	prop :: Property UnixLike
	prop = property' "ssh random host keys" $ \w -> do
		void $ liftIO $ boolSystem "sh"
			[ Param "-c"
			, Param "rm -f /etc/ssh/ssh_host_*"
			]
		ensureProperty w $ scriptProperty [ "DPKG_MAINTSCRIPT_NAME=postinst DPKG_MAINTSCRIPT_PACKAGE=openssh-server /var/lib/dpkg/info/openssh-server.postinst configure" ]
			`assume` MadeChange

-- | The text of a ssh public key, for example, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB3BJ2GqZiTR2LEoDXyYFgh/BduWefjdKXAsAtzS9zeI"
type PubKeyText = String

-- | Installs the specified list of ssh host keys.
--
-- The corresponding private keys come from the privdata.
--
-- Any host keys that are not in the list are removed from the host.
hostKeys :: IsContext c => c -> [(SshKeyType, PubKeyText)] -> Property (HasInfo + DebianLike)
hostKeys ctx l = go `before` cleanup
  where
	desc = "ssh host keys configured " ++ typelist (map fst l)
	go :: Property (HasInfo + DebianLike)
	go = propertyList desc $ toProps $ catMaybes $
		map (\(t, pub) -> Just $ hostKey ctx t pub) l
	typelist tl = "(" ++ unwords (map fromKeyType tl) ++ ")"
	alltypes = [minBound..maxBound]
	staletypes = let have = map fst l in filter (`notElem` have) alltypes
	removestale :: Bool -> [Property DebianLike]
	removestale b = map (tightenTargets . File.notPresent . flip keyFile b) staletypes
	cleanup :: Property DebianLike
	cleanup
		| null staletypes || null l = doNothing
		| otherwise =
			combineProperties ("any other ssh host keys removed " ++ typelist staletypes)
				(toProps $ removestale True ++ removestale False)
				`onChange` restarted

-- | Installs a single ssh host key of a particular type.
--
-- The public key is provided to this function;
-- the private key comes from the privdata;
hostKey :: IsContext c => c -> SshKeyType -> PubKeyText -> Property (HasInfo + DebianLike)
hostKey context keytype pub = go `onChange` restarted
  where
	go = combineProperties desc $ props
		& hostPubKey keytype pub
		& installpub
		& installpriv
	desc = "ssh host key configured (" ++ fromKeyType keytype ++ ")"
	keysrc ext field = PrivDataSourceFileFromCommand field ("sshkey"++ext)
		("ssh-keygen -t " ++ sshKeyTypeParam keytype ++ " -f sshkey")
	installpub :: Property UnixLike
	installpub = keywriter File.hasContent True (lines pub)
	installpriv :: Property (HasInfo + UnixLike)
	installpriv = withPrivData (keysrc "" (SshPrivKey keytype "")) context $ \getkey ->
		property' desc $ \w -> getkey $
			ensureProperty w
				. keywriter File.hasContentProtected False
				. privDataLines
	keywriter p ispub keylines = do
		let f = keyFile keytype ispub
		p f (keyFileContent keylines)

-- Make sure that there is a newline at the end;
-- ssh requires this for some types of private keys.
keyFileContent :: [String] -> [File.Line]
keyFileContent keylines = keylines ++ [""]

keyFile :: SshKeyType -> Bool -> FilePath
keyFile keytype ispub = "/etc/ssh/ssh_host_" ++ fromKeyType keytype ++ "_key" ++ ext
  where
	ext = if ispub then ".pub" else ""

-- | Indicates the host key that is used by a Host, but does not actually
-- configure the host to use it. Normally this does not need to be used;
-- use 'hostKey' instead.
hostPubKey :: SshKeyType -> PubKeyText -> Property (HasInfo + UnixLike)
hostPubKey t = pureInfoProperty "ssh pubkey known" . HostKeyInfo . M.singleton t

getHostPubKey :: Propellor (M.Map SshKeyType PubKeyText)
getHostPubKey = fromHostKeyInfo <$> askInfo

newtype HostKeyInfo = HostKeyInfo
	{ fromHostKeyInfo :: M.Map SshKeyType PubKeyText }
	deriving (Eq, Ord, Typeable, Show)

instance IsInfo HostKeyInfo where
	propagateInfo _ = PropagateInfo False

instance Sem.Semigroup HostKeyInfo where
	HostKeyInfo old <> HostKeyInfo new =
		-- new first because union prefers values from the first
		-- parameter when there is a duplicate key
		HostKeyInfo (new `M.union` old)

instance Monoid HostKeyInfo where
	mempty = HostKeyInfo M.empty
	mappend = (Sem.<>)

userPubKeys :: User -> [(SshKeyType, PubKeyText)] -> Property (HasInfo + UnixLike)
userPubKeys u@(User n) l = pureInfoProperty ("ssh pubkey for " ++ n) $
	UserKeyInfo (M.singleton u (S.fromList l))

getUserPubKeys :: User -> Propellor [(SshKeyType, PubKeyText)]
getUserPubKeys u = maybe [] S.toList . M.lookup u . fromUserKeyInfo <$> askInfo

newtype UserKeyInfo = UserKeyInfo
	{ fromUserKeyInfo :: M.Map User (S.Set (SshKeyType, PubKeyText)) }
	deriving (Eq, Ord, Typeable, Show)

instance IsInfo UserKeyInfo where
	propagateInfo _ = PropagateInfo False

instance Sem.Semigroup UserKeyInfo where
	UserKeyInfo old <> UserKeyInfo new =
		UserKeyInfo (M.unionWith S.union old new)

instance Monoid UserKeyInfo where
	mempty = UserKeyInfo M.empty
	mappend = (Sem.<>)

-- | Sets up a user with the specified public keys, and the corresponding
-- private keys from the privdata.
--
-- The public keys are added to the Info, so other properties like
-- `authorizedKeysFrom` can use them.
userKeys :: IsContext c => User -> c -> [(SshKeyType, PubKeyText)] -> Property (HasInfo + UnixLike)
userKeys user@(User name) context ks = combineProperties desc $ toProps $
	userPubKeys user ks : map (userKeyAt Nothing user context) ks
  where
	desc = unwords
		[ name
		, "has ssh key"
		, "(" ++ unwords (map (fromKeyType . fst) ks) ++ ")"
		]

-- | Sets up a user with the specified pubic key, and a private
-- key from the privdata.
--
-- A FilePath can be specified to write the key to somewhere other than
-- the default locations. Allows a user to have multiple keys for
-- different roles.
--
-- When the FilePath is relative, is put inside the User's 
-- ~/.ssh/ directory.
userKeyAt :: IsContext c => Maybe FilePath -> User -> c -> (SshKeyType, PubKeyText) -> Property (HasInfo + UnixLike)
userKeyAt dest user@(User u) context (keytype, pubkeytext) =
	combineProperties desc $ props
		& pubkey
		& privkey
  where
	desc = unwords $ catMaybes
		[ Just u
		, Just "has ssh key"
		, dest
		, Just $ "(" ++ fromKeyType keytype ++ ")"
		]
	pubkey :: Property UnixLike
	pubkey = property' desc $ \w -> 
		ensureProperty w =<< installprop File.hasContent ".pub" [pubkeytext]
	privkey :: Property (HasInfo + UnixLike)
	privkey = withPrivData (SshPrivKey keytype u) context privkey'
	privkey' :: ((PrivData -> Propellor Result) -> Propellor Result) -> Property (HasInfo + UnixLike)
	privkey' getkey = property' desc $ \w -> getkey $ \k ->
		ensureProperty w
			=<< installprop File.hasContentProtected "" (privDataLines k)
	installprop writer ext key = do
		f <- liftIO $ keyfile ext
		return $ combineProperties desc $ props
			& File.dirExists (takeDirectory f)
			& writer f (keyFileContent key)
			& File.ownerGroup f user (userGroup user)
			& File.ownerGroup (takeDirectory f) user (userGroup user)
	keyfile ext = case dest of
		Nothing -> relhomessh $ "id_" ++ fromKeyType keytype ++ ext
		Just f
			| isRelative f -> relhomessh (f ++ ext)
			| otherwise -> return (f ++ ext)
	relhomessh f = do
		home <- homeDirectory <$> getUserEntryForName u
		return $ home </> ".ssh" </> f

fromKeyType :: SshKeyType -> String
fromKeyType SshRsa = "rsa"
fromKeyType SshDsa = "dsa"
fromKeyType SshEcdsa = "ecdsa"
fromKeyType SshEd25519 = "ed25519"

-- | Puts some host's ssh public key(s), as set using `hostPubKey`
-- or `hostKey` into the known_hosts file for a user.
knownHost :: [Host] -> HostName -> User -> Property UnixLike
knownHost hosts hn user@(User u) = property' desc $ \w ->
	go w =<< knownHostLines hosts hn
  where
	desc = u ++ " knows ssh key for " ++ hn

	go _ [] = do
		warningMessage $ "no configured ssh host keys for " ++ hn
		return FailedChange
	go w ls = do
		f <- liftIO $ dotFile "known_hosts" user
		ensureProperty w $ modKnownHost user f $
			f `File.containsLines` ls
				`requires` File.dirExists (takeDirectory f)

-- | Reverts `knownHost`
unknownHost :: [Host] -> HostName -> User -> Property UnixLike
unknownHost hosts hn user@(User u) = property' desc $ \w ->
	go w =<< knownHostLines hosts hn
  where
	desc = u ++ " does not know ssh key for " ++ hn

	go _ [] = return NoChange
	go w ls = do
		f <- liftIO $ dotFile "known_hosts" user
		ifM (liftIO $ doesFileExist f)
			( ensureProperty w $ modKnownHost user f $
				f `File.lacksLines` ls
			, return NoChange
			)

knownHostLines :: [Host] -> HostName -> Propellor [File.Line]
knownHostLines hosts hn = keylines <$> fromHost hosts hn getHostPubKey
  where
	keylines (Just m) = map (\k -> hn ++ " " ++ k) (M.elems m)
	keylines Nothing = []

modKnownHost :: User -> FilePath -> Property UnixLike -> Property UnixLike
modKnownHost user f p = p
	`before` File.ownerGroup f user (userGroup user)
	`before` File.ownerGroup (takeDirectory f) user (userGroup user)

-- | Ensures that a local user's authorized_keys contains lines allowing
-- logins from a remote user on the specified Host.
--
-- The ssh keys of the remote user can be set using `userKeys`
--
-- Any other lines in the authorized_keys file are preserved as-is.
authorizedKeysFrom :: User -> (User, Host) -> Property UnixLike
localuser@(User ln) `authorizedKeysFrom` (remoteuser@(User rn), remotehost) =
	property' desc (\w -> go w =<< authorizedKeyLines remoteuser remotehost)
  where
	remote = rn ++ "@" ++ hostName remotehost
	desc = ln ++ " authorized_keys from " ++ remote

	go _ [] = do
		warningMessage $ "no configured ssh user keys for " ++ remote
		return FailedChange
	go w ls = ensureProperty w $ combineProperties desc $ toProps $
		map (setupRevertableProperty . authorizedKey localuser) ls

-- | Reverts `authorizedKeysFrom`
unauthorizedKeysFrom :: User -> (User, Host) -> Property UnixLike
localuser@(User ln) `unauthorizedKeysFrom` (remoteuser@(User rn), remotehost) =
	property' desc (\w -> go w =<< authorizedKeyLines remoteuser remotehost)
  where
	remote = rn ++ "@" ++ hostName remotehost
	desc = ln ++ " unauthorized_keys from " ++ remote

	go _ [] = return NoChange
	go w ls = ensureProperty w $ combineProperties desc $ toProps $
		map (undoRevertableProperty . authorizedKey localuser) ls

authorizedKeyLines :: User -> Host -> Propellor [File.Line]
authorizedKeyLines remoteuser remotehost =
	map snd <$> fromHost' remotehost (getUserPubKeys remoteuser)

-- | Makes a user have authorized_keys from the PrivData
--
-- This removes any other lines from the file.
authorizedKeys :: IsContext c => User -> c -> Property (HasInfo + UnixLike)
authorizedKeys user@(User u) context = withPrivData (SshAuthorizedKeys u) context $ \get ->
	property' desc $ \w -> get $ \v -> do
		f <- liftIO $ dotFile "authorized_keys" user
		ensureProperty w $ combineProperties desc $ props
			& File.hasContentProtected f (keyFileContent (privDataLines v))
			& File.ownerGroup f user (userGroup user)
			& File.ownerGroup (takeDirectory f) user (userGroup user)
  where
	desc = u ++ " has authorized_keys"

-- | Ensures that a user's authorized_keys contains a line.
-- Any other lines in the file are preserved as-is.
authorizedKey :: User -> String -> RevertableProperty UnixLike UnixLike
authorizedKey user@(User u) l = add <!> remove
  where
	add = property' (u ++ " has authorized_keys") $ \w -> do
		f <- liftIO $ dotFile "authorized_keys" user
		ensureProperty w $ modAuthorizedKey f user $
			f `File.containsLine` l
				`requires` File.dirExists (takeDirectory f)
	remove = property' (u ++ " lacks authorized_keys") $ \w -> do
		f <- liftIO $ dotFile "authorized_keys" user
		ifM (liftIO $ doesFileExist f)
			( ensureProperty w $ modAuthorizedKey f user $
				f `File.lacksLine` l
			, return NoChange
			)

modAuthorizedKey :: FilePath -> User -> Property UnixLike -> Property UnixLike
modAuthorizedKey f user p = p
	`before` File.mode f (combineModes [ownerWriteMode, ownerReadMode])
	`before` File.ownerGroup f user (userGroup user)
	`before` File.ownerGroup (takeDirectory f) user (userGroup user)
