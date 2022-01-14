module Propellor.Property.Apache where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.LetsEncrypt as LetsEncrypt

installed :: Property DebianLike
installed = Apt.installed ["apache2"]

restarted :: Property DebianLike
restarted = Service.restarted "apache2"

reloaded :: Property DebianLike
reloaded = Service.reloaded "apache2"

type ConfigLine = String

type ConfigFile = [ConfigLine]

siteEnabled :: Domain -> ConfigFile -> RevertableProperty DebianLike DebianLike
siteEnabled domain cf = siteEnabled' domain cf <!> siteDisabled domain

siteEnabled' :: Domain -> ConfigFile -> Property DebianLike
siteEnabled' domain cf = combineProperties ("apache site enabled " ++ domain) $ props
	& siteAvailable domain cf
		`requires` installed
		`onChange` reloaded
	& check (not <$> isenabled)
		(cmdProperty "a2ensite" ["--quiet", domain])
			`requires` installed
			`onChange` reloaded
  where
	isenabled = boolSystem "a2query" [Param "-q", Param "-s", Param domain]

siteDisabled :: Domain -> Property DebianLike
siteDisabled domain = combineProperties
	("apache site disabled " ++ domain)
	(toProps $ map File.notPresent (siteCfg domain))
		`onChange` (cmdProperty "a2dissite" ["--quiet", domain] `assume` MadeChange)
		`requires` installed
		`onChange` reloaded

siteAvailable :: Domain -> ConfigFile -> Property DebianLike
siteAvailable domain cf = combineProperties ("apache site available " ++ domain) $
	toProps $ map tightenTargets $
		map (`File.hasContent` (comment:cf)) (siteCfg domain)
  where
	comment = "# deployed with propellor, do not modify"

modEnabled :: String -> RevertableProperty DebianLike DebianLike
modEnabled modname = enable <!> disable
  where
	enable = check (not <$> isenabled)
		(cmdProperty "a2enmod" ["--quiet", modname])
			`describe` ("apache module enabled " ++ modname)
			`requires` installed
			`onChange` reloaded
	disable = check isenabled
		(cmdProperty "a2dismod" ["--quiet", modname])
			`describe` ("apache module disabled " ++ modname)
			`requires` installed
			`onChange` reloaded
	isenabled = boolSystem "a2query" [Param "-q", Param "-m", Param modname]

-- | Control whether an apache configuration file is enabled. 
--
-- The String is the base name of the configuration, eg "charset" or "gitweb".
confEnabled :: String -> RevertableProperty DebianLike DebianLike
confEnabled confname = enable <!> disable
  where
	enable = check (not <$> isenabled)
		(cmdProperty "a2enconf" ["--quiet", confname])
			`describe` ("apache configuration enabled " ++ confname)
			`requires` installed
			`onChange` reloaded
	disable = check isenabled
		(cmdProperty "a2disconf" ["--quiet", confname])
			`describe` ("apache configuration disabled " ++ confname)
			`requires` installed
			`onChange` reloaded
	isenabled = boolSystem "a2query" [Param "-q", Param "-c", Param confname]

-- | Make apache listen on the specified ports.
--
-- Note that ports are also specified inside a site's config file,
-- so that also needs to be changed.
listenPorts :: [Port] -> Property DebianLike
listenPorts ps = "/etc/apache2/ports.conf" `File.hasContent` map portline ps
	`onChange` restarted
  where
	portline port = "Listen " ++ val port

-- This is a list of config files because different versions of apache
-- use different filenames. Propellor simply writes them all.
siteCfg :: Domain -> [FilePath]
siteCfg domain =
	-- Debian pre-2.4
	[ "/etc/apache2/sites-available/" ++ domain
	-- Debian 2.4+
	, "/etc/apache2/sites-available/" ++ domain ++ ".conf"
	]

-- | Configure apache to use SNI to differentiate between
-- https hosts.
--
-- This was off by default in apache 2.2.22. Newver versions enable
-- it by default. This property uses the filename used by the old version.
multiSSL :: Property DebianLike
multiSSL = check (doesDirectoryExist "/etc/apache2/conf.d") $
	"/etc/apache2/conf.d/ssl" `File.hasContent`
		[ "NameVirtualHost *:443"
		, "SSLStrictSNIVHostCheck off"
		]
		`describe` "apache SNI enabled"
		`onChange` reloaded

-- | Config file fragment that can be inserted into a <Directory>
-- stanza to allow global read access to the directory.
--
-- Works with multiple versions of apache that have different ways to do
-- it.
allowAll :: ConfigLine
allowAll = unlines
	[ "<IfVersion < 2.4>"
	, "Order allow,deny"
	, "allow from all"
	, "</IfVersion>"
	, "<IfVersion >= 2.4>"
	, "Require all granted"
	, "</IfVersion>"
	]

-- | Config file fragment that can be inserted into a <VirtualHost>
-- stanza to allow apache to display directory index icons.
iconDir :: ConfigLine
iconDir = unlines
	[ "<Directory \"/usr/share/apache2/icons\">"
	, "Options Indexes MultiViews"
	, "AllowOverride None"
	, allowAll
	, "  </Directory>"
	]

type WebRoot = FilePath

-- | A basic virtual host, publishing a directory, and logging to
-- the combined apache log file. Not https capable.
virtualHost :: Domain -> Port -> WebRoot -> RevertableProperty DebianLike DebianLike
virtualHost domain port docroot = virtualHost' domain port docroot []

-- | Like `virtualHost` but with additional config lines added.
virtualHost' :: Domain -> Port -> WebRoot -> [ConfigLine] -> RevertableProperty DebianLike DebianLike
virtualHost' domain port docroot addedcfg = siteEnabled domain $
	[ "<VirtualHost *:" ++ val port ++ ">"
	, "ServerName " ++ domain ++ ":" ++ val port
	, "DocumentRoot " ++ docroot
	, "ErrorLog /var/log/apache2/error.log"
	, "LogLevel warn"
	, "CustomLog /var/log/apache2/access.log combined"
	, "ServerSignature On"
	]
	++ addedcfg ++
	[ "</VirtualHost>"
	]

-- | A virtual host using https, with the certificate obtained
-- using `Propellor.Property.LetsEncrypt.letsEncrypt`.
--
-- http connections are redirected to https.
--
-- Example:
--
-- > httpsVirtualHost "example.com" "/var/www"
-- > 	(LetsEncrypt.AgreeTOS (Just "me@my.domain"))
--
-- Note that reverting this property does not remove the certificate from
-- letsencrypt's cert store.
httpsVirtualHost :: Domain -> WebRoot -> LetsEncrypt.AgreeTOS -> RevertableProperty DebianLike DebianLike
httpsVirtualHost domain docroot letos = httpsVirtualHost' domain docroot letos []

-- | Like `httpsVirtualHost` but with additional config lines added.
httpsVirtualHost' :: Domain -> WebRoot -> LetsEncrypt.AgreeTOS -> [ConfigLine] -> RevertableProperty DebianLike DebianLike
httpsVirtualHost' domain docroot letos addedcfg = setup <!> teardown
  where
	setup = setuphttp
		`requires` modEnabled "rewrite"
		`requires` modEnabled "ssl"
		`before` setuphttps
	teardown = siteDisabled domain
	setuphttp = (siteEnabled' domain $
		-- The sslconffile is only created after letsencrypt gets
		-- the cert. The "*" is needed to make apache not error
		-- when the file doesn't exist.
		("IncludeOptional " ++ sslconffile "*")
		: vhost (Port 80)
			[ "RewriteEngine On"
			-- Pass through .well-known directory on http for the
			-- letsencrypt acme challenge.
			, "RewriteRule ^/.well-known/(.*) - [L]"
			-- Everything else redirects to https
			, "RewriteRule ^/(.*) https://" ++ domain ++ "/$1 [L,R,NE]"
			])
		`requires` File.dirExists (takeDirectory cf)
	setuphttps = LetsEncrypt.letsEncrypt letos domain docroot
		`onChange` postsetuphttps
	postsetuphttps = combineProperties (domain ++ " ssl cert installed") $ props
		& File.hasContent cf sslvhost
			`onChange` reloaded
		-- always reload since the cert has changed
		& reloaded
	  where
		sslvhost = vhost (Port 443)
			[ "SSLEngine on"
			, "SSLCertificateFile " ++ LetsEncrypt.certFile domain
			, "SSLCertificateKeyFile " ++ LetsEncrypt.privKeyFile domain
			, "SSLCertificateChainFile " ++ LetsEncrypt.chainFile domain
			]
	cf = sslconffile "letsencrypt"
	sslconffile s = "/etc/apache2/sites-available/ssl/" ++ domain ++ "/" ++ s ++ ".conf"
	vhost p ls =
		[ "<VirtualHost *:" ++ val p ++">"
		, "ServerName " ++ domain ++ ":" ++ val p
		, "DocumentRoot " ++ docroot
		, "ErrorLog /var/log/apache2/error.log"
		, "LogLevel warn"
		, "CustomLog /var/log/apache2/access.log combined"
		, "ServerSignature On"
		] ++ ls ++ addedcfg ++
		[ "</VirtualHost>"
		]
