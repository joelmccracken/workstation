{-# Language ScopedTypeVariables #-}

module Propellor.Spin (
	commitSpin,
	spin,
	spin',
	update,
	gitPushHelper,
	mergeSpin,
) where

import Data.List
import System.Exit
import System.PosixCompat
import System.Posix.IO
import System.Posix.Directory
import Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified Data.Set as S
import Network.Socket (getAddrInfo, defaultHints, AddrInfo(..), AddrInfoFlag(..), SockAddr)

import Propellor.Base
import Propellor.Protocol
import Propellor.PrivData.Paths
import Propellor.Git
import Propellor.Git.Config
import Propellor.Ssh
import Propellor.Gpg
import Propellor.Bootstrap
import Propellor.Types.CmdLine
import Propellor.Types.Info
import Propellor.Property.Localdir (OriginUrl(..))
import qualified Propellor.Shim as Shim
import Utility.SafeCommand
import Utility.Process.NonConcurrent

commitSpin :: IO ()
commitSpin = do
	-- safety check #1: check we're on the configured spin branch
	spinBranch <- getGitConfigValue "propellor.spin-branch"
	case spinBranch of
		Nothing -> return () -- just a noop
		Just b -> do
			currentBranch <- getCurrentBranch
			when (b /= currentBranch) $
				error ("spin aborted: check out "
					++ b ++ " branch first")

	-- safety check #2: check we can commit with a dirty tree
	noDirtySpin <- getGitConfigBool "propellor.forbid-dirty-spin"
	when noDirtySpin $ do
		status <- takeWhile (/= '\n')
			<$> readProcess "git" ["status", "--porcelain"]
		when (not . null $ status) $
			error "spin aborted: commit changes first"

	void $ actionMessage "Git commit" $
		gitCommit (Just spinCommitMessage)
			[Param "--allow-empty", Param "-a"]
	-- Push to central origin repo first, if possible.
	-- The remote propellor will pull from there, which avoids
	-- us needing to send stuff directly to the remote host.
	whenM hasOrigin $
		void $ actionMessage "Push to central git repository" $
			boolSystemNonConcurrent "git" [Param "push"]

spin :: Maybe HostName -> HostName -> Host -> IO ()
spin = spin' Nothing

spin' :: Maybe PrivMap -> Maybe HostName -> HostName -> Host -> IO ()
spin' mprivdata relay target hst = do
	cacheparams <- if viarelay
		then pure ["-A"]
		else toCommand <$> sshCachingParams hn
	when viarelay $
		void $ boolSystem "ssh-add" []

	sshtarget <- ("root@" ++) <$> case relay of
		Just r -> pure r
		Nothing -> getSshTarget target hst

	-- Install, or update the remote propellor.
	updateServer target relay hst
		(proc "ssh" $ cacheparams ++ [sshtarget, shellWrap probecmd])
		(proc "ssh" $ cacheparams ++ [sshtarget, shellWrap updatecmd])
		=<< getprivdata

	-- And now we can run it.
	unlessM (boolSystemNonConcurrent "ssh" (map Param $ cacheparams ++ ["-t", sshtarget, shellWrap runcmd])) $
		giveup "remote propellor failed"
  where
	hn = fromMaybe target relay
	sys = case fromInfo (hostInfo hst) of
		InfoVal o -> Just o
		NoInfoVal -> Nothing
	bootstrapper = case fromInfo (hostInfo hst) of
		NoInfoVal -> defaultBootstrapper
		InfoVal bs -> bs

	relaying = relay == Just target
	viarelay = isJust relay && not relaying

	probecmd = intercalate " ; "
		[ "if [ ! -d " ++ localdir ++ "/.git ]"
		, "then (" ++ intercalate " && "
			[ installGitCommand sys
			, "echo " ++ toMarked statusMarker (show NeedGitClone)
			] ++ ") || echo " ++ toMarked statusMarker (show NeedPrecompiled)
		, "else " ++ updatecmd
		, "fi"
		]

	updatecmd = intercalate " && "
		[ "cd " ++ localdir
		, bootstrapPropellorCommand bootstrapper sys
		, if viarelay
			then "./propellor --continue " ++
				shellEscape (show (Relay target))
			-- Still using --boot for back-compat...
			else "./propellor --boot " ++ target
		]

	runcmd = "cd " ++ localdir ++ " && ./propellor " ++ cmd
	cmd = "--serialized " ++ shellEscape (show cmdline)
	cmdline
		| viarelay = Spin [target] (Just target)
		| otherwise = SimpleRun target

	getprivdata = case mprivdata of
		Nothing
			| relaying -> do
				let f = privDataRelay hn
				d <- readPrivDataFile f
				nukeFile f
				return d
			| otherwise ->
				filterPrivData hst <$> decryptPrivData
		Just pd -> pure pd

-- Check if the Host contains an IP address that matches one of the IPs
-- in the DNS for the HostName. If so, the HostName is used as-is,
-- but if the DNS is out of sync with the Host config, or doesn't have
-- the host in it at all, use one of the Host's IPs instead.
getSshTarget :: HostName -> Host -> IO String
getSshTarget target hst
	| null configips = return target
	| otherwise = go =<< tryIO (dnslookup target)
  where
	go (Left e) = useip (show e)
	go (Right addrinfos) = do
		configaddrinfos <- catMaybes <$> mapM iptoaddr configips
		if any (`elem` configaddrinfos) (map addrAddress addrinfos)
			then return target
			else useip ("DNS lookup did not return any of the expected addresses " ++ show configips)

	dnslookup h = getAddrInfo (Just $ defaultHints { addrFlags = [AI_CANONNAME] }) (Just h) Nothing

	-- Convert a string containing an IP address into a SockAddr.
	iptoaddr :: String -> IO (Maybe SockAddr)
	iptoaddr ip = catchDefaultIO Nothing $ headMaybe . map addrAddress
		<$> getAddrInfo (Just $ defaultHints { addrFlags = [AI_NUMERICHOST] })  (Just ip) Nothing

	useip why = case headMaybe configips of
		Nothing -> return target
		Just ip -> do
			-- If we're being asked to run on the local host,
			-- ignore DNS.
			s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
			if s == target
				then return target
				else do
					warningMessage $ "DNS seems out of date for " ++ target ++ " (" ++ why ++ "); using IP address from configuration instead."
					return ip

	configips = map val $ mapMaybe getIPAddr $
		S.toList $ getDnsInfo $ hostInfo hst

-- Update the privdata, repo url, and git repo over the ssh
-- connection, talking to the user's local propellor instance which is
-- running the updateServer
update :: Maybe HostName -> IO ()
update forhost = do
	whenM hasGitRepo $
		req NeedRepoUrl repoUrlMarker setRepoUrl

	makePrivDataDir
	createDirectoryIfMissing True (takeDirectory privfile)
	req NeedPrivData privDataMarker $
		writeFileProtected privfile

	whenM hasGitRepo $
		gitPullFromUpdateServer
  where
	-- When --spin --relay is run, get a privdata file
	-- to be relayed to the target host.
	privfile = maybe privDataLocal privDataRelay forhost

updateServer
	:: HostName
	-> Maybe HostName
	-> Host
	-> CreateProcess
	-> CreateProcess
	-> PrivMap
	-> IO ()
updateServer target relay hst connect haveprecompiled privdata = do
	(Just toh, Just fromh, _, pid) <- createProcessNonConcurrent $ connect
		{ std_in = CreatePipe
		, std_out = CreatePipe
		}
	go (toh, fromh)
	forceSuccessProcess' connect =<< waitForProcessNonConcurrent pid
  where
	hn = fromMaybe target relay

	go (toh, fromh) = do
		let loop = go (toh, fromh)
		let restart = updateServer hn relay hst connect haveprecompiled privdata
		let done = return ()
		v <- maybe Nothing readish <$> getMarked fromh statusMarker
		case v of
			(Just NeedRepoUrl) -> do
				sendRepoUrl hst toh
				loop
			(Just NeedPrivData) -> do
				sendPrivData hn toh privdata
				loop
			(Just NeedGitClone) -> do
				hClose toh
				hClose fromh
				sendGitClone hn
				restart
			(Just NeedPrecompiled) -> do
				hClose toh
				hClose fromh
				sendPrecompiled hn
				updateServer hn relay hst haveprecompiled (error "loop") privdata
			(Just NeedGitPush) -> do
				sendGitUpdate hn fromh toh
				hClose fromh
				hClose toh
				done
			Nothing -> done

sendRepoUrl :: Host -> Handle -> IO ()
sendRepoUrl hst toh = sendMarked toh repoUrlMarker =<< geturl
  where
	geturl = case fromInfoVal (fromInfo (hostInfo hst)) of
		Nothing -> fromMaybe "" <$> getRepoUrl
		Just (OriginUrl u) -> return u

sendPrivData :: HostName -> Handle -> PrivMap -> IO ()
sendPrivData hn toh privdata = void $ actionMessage msg $ do
	sendMarked toh privDataMarker d
	return True
  where
	msg = "Sending privdata (" ++ show (length d) ++ " bytes) to " ++ hn
	d = show privdata

sendGitUpdate :: HostName -> Handle -> Handle -> IO ()
sendGitUpdate hn fromh toh =
	void $ actionMessage ("Sending git update to " ++ hn) $ do
		sendMarked toh gitPushMarker ""
		(Nothing, Nothing, Nothing, h) <- createProcess p
		(==) ExitSuccess <$> waitForProcess h
  where
	p = (proc "git" ["upload-pack", "."])
		{ std_in = UseHandle fromh
		, std_out = UseHandle toh
		}

-- Initial git clone, used for bootstrapping.
sendGitClone :: HostName -> IO ()
sendGitClone hn = void $ actionMessage ("Clone git repository to " ++ hn) $ do
	branch <- getCurrentBranch
	cacheparams <- sshCachingParams hn
	withTmpFile "propellor.git" $ \tmp _ -> allM id
		[ boolSystem "git" [Param "bundle", Param "create", File tmp, Param "HEAD"]
		, boolSystemNonConcurrent "scp" $ cacheparams ++ [File tmp, Param ("root@"++hn++":"++remotebundle)]
		, boolSystemNonConcurrent "ssh" $ cacheparams ++ [Param ("root@"++hn), Param $ unpackcmd branch]
		]
  where
	remotebundle = "/usr/local/propellor.git"
	unpackcmd branch = shellWrap $ intercalate " && "
		[ "git clone " ++ remotebundle ++ " " ++ localdir
		, "cd " ++ localdir
		, "git checkout -b " ++ branch
		, "git remote rm origin"
		, "rm -f " ++ remotebundle
		]

-- Send a tarball containing the precompiled propellor, and libraries.
-- This should be reasonably portable, as long as the remote host has the
-- same architecture as the build host.
sendPrecompiled :: HostName -> IO ()
sendPrecompiled hn = void $ actionMessage "Uploading locally compiled propellor as a last resort" $
	bracket getWorkingDirectory changeWorkingDirectory $ \_ ->
		withTmpDir "propellor" go
  where
	go tmpdir = do
		cacheparams <- sshCachingParams hn
		let shimdir = takeFileName localdir
		createDirectoryIfMissing True (tmpdir </> shimdir)
		changeWorkingDirectory (tmpdir </> shimdir)
		me <- readSymbolicLink "/proc/self/exe"
		createDirectoryIfMissing True "bin"
		unlessM (boolSystem "cp" [File me, File "bin/propellor"]) $
			errorMessage "failed copying in propellor"
		let bin = "bin/propellor"
		let binpath = Just $ localdir </> bin
		void $ Shim.setup bin binpath "."
		changeWorkingDirectory tmpdir
		withTmpFile "propellor.tar." $ \tarball _ -> allM id
			[ boolSystem "strip" [File me]
			, boolSystem "tar" [Param "czf", File tarball, File shimdir]
			, boolSystemNonConcurrent "scp" $ cacheparams ++ [File tarball, Param ("root@"++hn++":"++remotetarball)]
			, boolSystemNonConcurrent "ssh" $ cacheparams ++ [Param ("root@"++hn), Param unpackcmd]
			]

	remotetarball = "/usr/local/propellor.tar"

	unpackcmd = shellWrap $ intercalate " && "
		[ "cd " ++ takeDirectory remotetarball
		, "tar xzf " ++ remotetarball
		, "rm -f " ++ remotetarball
		]

mergeSpin :: IO ()
mergeSpin = do
	branch <- getCurrentBranch
	branchref <- getCurrentBranchRef
	old_head <- getCurrentGitSha1 branch
	old_commit <- findLastNonSpinCommit
	rungit "reset" [Param old_commit]
	unlessM (gitCommit Nothing [Param "-a", Param "--allow-empty"]) $
		error "git commit failed"
	rungit "merge" =<< gpgSignParams [Param "-s", Param "ours", Param old_head, Param "--no-edit"]
	current_commit <- getCurrentGitSha1 branch
	rungit "update-ref" [Param branchref, Param current_commit]
	rungit "checkout" [Param branch]
  where
	rungit cmd ps = unlessM (boolSystem "git" (Param cmd:ps)) $
		error ("git " ++ cmd ++ " failed")

findLastNonSpinCommit :: IO String
findLastNonSpinCommit = do
	commits <- map (separate (== ' ')) . lines
		<$> readProcess "git" ["log", "--oneline", "--no-abbrev-commit"]
	case dropWhile (\(_, msg) -> msg == spinCommitMessage) commits of
		((sha, _):_) -> return sha
		_ -> error $ "Did not find any previous commit that was not a " ++ show spinCommitMessage

spinCommitMessage :: String
spinCommitMessage = "propellor spin"

-- Stdin and stdout are connected to the updateServer over ssh.
-- Request that it run git upload-pack, and connect that up to a git fetch
-- to receive the data.
gitPullFromUpdateServer :: IO ()
gitPullFromUpdateServer = req NeedGitPush gitPushMarker $ \_ -> do
	-- IO involving stdin can cause data to be buffered in the Handle
	-- (even when it's set NoBuffering), but we need to pass a FD to 
	-- git fetch containing all of stdin after the gitPushMarker,
	-- including any that has been buffered.
	--
	-- To do so, create a pipe, and forward stdin, including any
	-- buffered part, through it.
	(pread, pwrite) <- System.Posix.IO.createPipe
	-- Note that there is a race between the createPipe and setting
	-- CloseOnExec. Another processess forked here would inherit
	-- pwrite and perhaps keep it open. However, propellor is not
	-- running concurrent threads at this point, so this is ok.
	setFdOption pwrite CloseOnExec True
	hwrite <- fdToHandle pwrite
	forwarder <- async $ stdin *>* hwrite
	let hin = pread
	hout <- dup stdOutput
	hClose stdout
	-- Not using git pull because git 2.5.0 badly
	-- broke its option parser.
	unlessM (boolSystemNonConcurrent "git" (fetchparams hin hout)) $
		errorMessage "git fetch from client failed"
	wait forwarder
	unlessM (boolSystemNonConcurrent "git" [Param "merge", Param "FETCH_HEAD"]) $
		errorMessage "git merge from client failed"
  where
	fetchparams hin hout =
		[ Param "fetch"
		, Param "--progress"
		, Param "--upload-pack"
		, Param $ "./propellor --gitpush " ++ show hin ++ " " ++ show hout
		, Param "."
		]

-- Shim for git push over the propellor ssh channel.
-- Reads from stdin and sends it to hout;
-- reads from hin and sends it to stdout.
gitPushHelper :: Fd -> Fd -> IO ()
gitPushHelper hin hout = void $ fromstdin `concurrently` tostdout
  where
	fromstdin = do
		h <- fdToHandle hout
		stdin *>* h
	tostdout = do
		h <- fdToHandle hin
		h *>* stdout

-- Forward data from one handle to another.
(*>*) :: Handle -> Handle -> IO ()
fromh *>* toh = do
	b <- B.hGetSome fromh 40960
	if B.null b
		then do
			hClose fromh
			hClose toh
		else do
			B.hPut toh b
			hFlush toh
			fromh *>* toh
