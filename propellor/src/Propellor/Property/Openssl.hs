-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Openssl where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import Utility.SafeCommand

installed :: Property DebianLike
installed = Apt.installed ["openssl"]

dhparamsLength :: Int
dhparamsLength = 2048

dhparams :: FilePath
dhparams = "/etc/ssl/private/dhparams.pem"

safeDhparams :: Property DebianLike
safeDhparams = propertyList "safe dhparams" $ props
	& File.dirExists (takeDirectory dhparams)
	& installed
	& check (not <$> doesFileExist dhparams) (createDhparams dhparams dhparamsLength)

createDhparams :: FilePath -> Int -> Property UnixLike
createDhparams f l = property ("generate new dhparams: " ++ f) $ liftIO $ withUmask 0o0177 $ withFile f WriteMode $ \h ->
	cmdResult <$> boolSystem' "openssl" [Param "dhparam", Param (show l)] (\p -> p { std_out = UseHandle h })
