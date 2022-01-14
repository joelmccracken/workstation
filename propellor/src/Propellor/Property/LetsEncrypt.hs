-- | This module gets LetsEncrypt <https://letsencrypt.org/> certificates 
-- using CertBot <https://certbot.eff.org/>

module Propellor.Property.LetsEncrypt where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt

import System.Posix.Files

installed :: Property DebianLike
installed = Apt.installed ["certbot"]

-- | Tell the letsencrypt client that you agree with the Let's Encrypt
-- Subscriber Agreement. Providing an email address is recommended,
-- so that letcencrypt can contact you about problems.
data AgreeTOS = AgreeTOS (Maybe Email)

type Email = String

type WebRoot = FilePath

-- | Uses letsencrypt to obtain a certificate for a domain.
--
-- This should work with any web server, as long as letsencrypt can
-- write its temp files to the web root. The letsencrypt client does
-- not modify the web server's configuration in any way; this only obtains
-- the certificate it does not make the web server use it.
-- 
-- This also handles renewing the certificate.
-- For renewel to work well, propellor needs to be
-- run periodically (at least a couple times per month).
--
-- This property returns `MadeChange` when the certificate is initially
-- obtained, and when it's renewed. So, it can be combined with a property
-- to make the webserver (or other server) use the certificate:
--
-- > letsEncrypt (AgreeTOS (Just "me@example.com")) "example.com" "/var/www"
-- > 	`onChange` Apache.reload
--
-- See `Propellor.Property.Apache.httpsVirtualHost` for a more complete
-- integration of apache with letsencrypt, that's built on top of this.
letsEncrypt :: AgreeTOS -> Domain -> WebRoot -> Property DebianLike
letsEncrypt tos domain = letsEncrypt' tos domain []

-- | Like `letsEncrypt`, but the certificate can be obtained for multiple
-- domains.
letsEncrypt' :: AgreeTOS -> Domain -> [Domain] -> WebRoot -> Property DebianLike
letsEncrypt' (AgreeTOS memail) domain domains webroot =
	prop `requires` installed
  where
	prop :: Property UnixLike
	prop = property desc $ do
		startstats <- liftIO getstats
		(transcript, ok) <- liftIO $
			processTranscript "letsencrypt" params Nothing
		if ok
			then do
				endstats <- liftIO getstats
				if startstats /= endstats
					then return MadeChange
					else return NoChange
			else do
				liftIO $ hPutStr stderr transcript
				return FailedChange
	
	desc = "letsencrypt " ++ unwords alldomains
	alldomains = domain : domains
	params =
		[ "certonly"
		, "--agree-tos"
		, case memail of
			Just email -> "--email="++email
			Nothing -> "--register-unsafely-without-email"
		, "--webroot"
		, "--webroot-path", webroot
		, "--text"
		, "--noninteractive"
		, "--keep-until-expiring"
		-- The list of domains may be changed, adding more, so
		-- always request expansion.
		, "--expand"
		] ++ map (\d -> "--domain="++d) alldomains

	getstats = mapM statcertfiles alldomains
	statcertfiles d = mapM statfile
		[ certFile d
		, privKeyFile d
		, chainFile d
		, fullChainFile d
		]
	statfile f = catchMaybeIO $ do
		s <- getFileStatus f
		return (fileID s, deviceID s, fileMode s, fileSize s, modificationTime s)

-- | The cerificate files that letsencrypt will make available for a domain.
liveCertDir :: Domain -> FilePath
liveCertDir d = "/etc/letsencrypt/live" </> d

certFile :: Domain -> FilePath
certFile d = liveCertDir d </> "cert.pem"

privKeyFile :: Domain -> FilePath
privKeyFile d = liveCertDir d </> "privkey.pem"

chainFile :: Domain -> FilePath
chainFile d = liveCertDir d </> "chain.pem"

fullChainFile :: Domain -> FilePath
fullChainFile d = liveCertDir d </> "fullchain.pem"
