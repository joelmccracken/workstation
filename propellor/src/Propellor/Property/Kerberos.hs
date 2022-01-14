-- | Maintainer: Jelmer Vernooij <jelmer@samba.org>

module Propellor.Property.Kerberos where

import Utility.Process

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import Propellor.Property.User

type Realm = String
type Principal = String
type Kvno = Integer

-- Standard paths in MIT Kerberos

defaultKeyTab :: FilePath
defaultKeyTab = "/etc/krb5.keytab"

kadmAclPath :: FilePath
kadmAclPath = "/etc/krb5kdc/kadm5.acl"

kpropdAclPath :: FilePath
kpropdAclPath = "/etc/krb5kdc/kpropd.acl"

kdcConfPath :: FilePath
kdcConfPath = "/etc/krb5kdc/kdc.conf"

keyTabPath :: Maybe FilePath -> FilePath
keyTabPath = maybe defaultKeyTab id

-- | Create a principal from a primary, instance and realm
principal :: String -> Maybe String -> Maybe Realm -> Principal
principal p i r = p ++ maybe "" ("/"++) i ++ maybe "" ("@" ++) r

installed :: Property DebianLike
installed = Apt.installed ["krb5-user"]

kdcInstalled :: Property DebianLike
kdcInstalled = Apt.serviceInstalledRunning "krb5-kdc"

adminServerInstalled :: Property DebianLike
adminServerInstalled = Apt.serviceInstalledRunning "krb5-admin-server"

kpropServerInstalled :: Property DebianLike
kpropServerInstalled = propertyList "kprop server installed" $ props
	& kdcInstalled
	& Apt.installed ["openbsd-inetd"]
	& "/etc/inetd.conf" `File.containsLines`
		[ "krb5_prop\tstream\ttcp\tnowait\troot\t/usr/sbin/kpropd kpropd"
		, "krb5_prop\tstream\ttcp6\tnowait\troot\t/usr/sbin/kpropd kpropd"
		]

kpropAcls :: [String] -> Property UnixLike
kpropAcls ps = kpropdAclPath `File.hasContent` ps `describe` "kprop server ACLs"

k5srvutil :: (Maybe FilePath) -> [String] -> IO String
k5srvutil kt cmd = readProcess "k5srvutil" (maybe [] (\x -> ["-f", x]) kt ++ cmd)

-- Keytab management
keytabEntries :: Maybe FilePath -> IO [(Kvno, Principal)]
keytabEntries p = do
	c <- k5srvutil p ["list"]
	return $ map parseLine (drop 3 $ lines c)
  where
	parseLine l = (Prelude.read x, y) where (x, y) = splitAt 5 l

checkKeyTabEntry' :: Maybe FilePath -> (Kvno, Principal) -> IO Bool
checkKeyTabEntry' path entry = do
	entries <- keytabEntries path
	return $ entry `elem` entries

checkKeyTabEntry :: Maybe FilePath -> Principal -> IO Bool
checkKeyTabEntry path princ = do
	entries <- keytabEntries path
	return $ princ `elem` (map snd entries)

-- k5login files
k5loginPath :: User -> IO FilePath
k5loginPath user = do
	h <- homedir user
	return $ h </> ".k5login"

k5login :: User -> [Principal] -> Property UnixLike
k5login user@(User u) ps = property' desc $ \w -> do
	f <- liftIO $ k5loginPath user
	liftIO $ do
		createDirectoryIfMissing True (takeDirectory f)
		writeFile f (unlines ps)
	ensureProperty w $ combineProperties desc $ props
		& File.ownerGroup f user (userGroup user)
		& File.ownerGroup (takeDirectory f) user (userGroup user)
  where
	desc = u ++ " has k5login"
