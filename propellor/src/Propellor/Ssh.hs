module Propellor.Ssh where

import Propellor.Base
import Utility.UserInfo
import Utility.FileSystemEncoding

import System.PosixCompat
import Data.Time.Clock.POSIX
import Data.Hashable

-- Parameters can be passed to both ssh and scp, to enable a ssh connection
-- caching socket.
--
-- If the socket already exists, check if its mtime is older than 10
-- minutes, and if so stop that ssh process, in order to not try to
-- use an old stale connection. (atime would be nicer, but there's
-- a good chance a laptop uses noatime)
sshCachingParams :: HostName -> IO [CommandParam]
sshCachingParams hn = do
	home <- myHomeDir
	let socketfile = socketFile home hn
	createDirectoryIfMissing True (takeDirectory socketfile)
	let ps =
		[ Param "-o"
		, Param ("ControlPath=" ++ socketfile)
		, Param "-o", Param "ControlMaster=auto"
		, Param "-o", Param "ControlPersist=yes"
		]

	maybe noop (expireold ps socketfile)
		=<< catchMaybeIO (getFileStatus socketfile)
	
	return ps
		
  where
	expireold ps f s = do
		now <- truncate <$> getPOSIXTime :: IO Integer
		if modificationTime s > fromIntegral now - tenminutes
			then touchFile f
			else do
				void $ boolSystem "ssh" $
					[ Param "-O", Param "stop" ] ++ ps ++
					[ Param "localhost" ]
				nukeFile f
	tenminutes = 600

-- Generate a socket filename inside the home directory.
--
-- There's a limit in the size of unix domain sockets, of approximately
-- 100 bytes. Try to never construct a filename longer than that.
--
-- When space allows, include the full hostname in the socket filename.
-- Otherwise, a checksum of the hostname is included in the name, to
-- avoid using the same socket file for multiple hosts.
socketFile :: FilePath -> HostName -> FilePath
socketFile home hn = selectSocketFile
	[ sshdir </> hn ++ ".sock"
	, sshdir </> hn
	, sshdir </> take 10 hn ++ "-" ++ checksum
	, sshdir </> checksum
	]
	(home </> ".propellor-" ++ checksum)
  where
	sshdir = home </> ".ssh" </> "propellor"
	checksum = take 9 $ show $ abs $ hash hn

selectSocketFile :: [FilePath] -> FilePath -> FilePath
selectSocketFile [] d = d
selectSocketFile (f:fs) d
	| valid_unix_socket_path f = f
	| otherwise = selectSocketFile fs d

valid_unix_socket_path :: FilePath -> Bool
valid_unix_socket_path f = length (decodeW8 f) < 100 - reservedbyssh
  where
	-- ssh tacks on 17 or so characters when making a socket
	reservedbyssh = 18
