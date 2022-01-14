module Propellor.CmdLine (
	defaultMain,
	processCmdLine,
) where

import System.Environment (getArgs)
import Data.List
import System.Exit
import System.PosixCompat
import Network.Socket

import Propellor.Base
import Propellor.Gpg
import Propellor.Git
import Propellor.Git.VerifiedBranch
import Propellor.Bootstrap
import Propellor.Spin
import Propellor.Types.CmdLine
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Shim as Shim
import Utility.FileSystemEncoding

usage :: Handle -> IO ()
usage h = hPutStrLn h $ unlines
	[ "Usage:"
	, "  with no arguments, provision the current host"
	, ""
	, "  --init"
	, "      initialize ~/.propellor"
	, "  hostname"
	, "      provision the current host as if it had the specified hostname"
	, "  --spin targethost [--via relayhost]"
	, "      provision the specified host"
	, "  --build"
	, "      recompile using your current config"
	, "  --add-key keyid"
	, "      add an additional signing key to the private data"
	, "  --rm-key keyid"
	, "      remove a signing key from the private data"
	, "  --list-fields"
	, "      list private data fields"
	, "  --set field context"
	, "      set a private data field"
	, "  --unset field context"
	, "      clear a private data field"
	, "  --unset-unused"
	, "      clear unused fields from the private data"
	, "  --dump field context"
	, "      show the content of a private data field"
	, "  --edit field context"
	, "      edit the content of a private data field"
	, "  --merge"
	, "      combine multiple spins into a single git commit"
	, "  --check"
	, "      double-check that propellor can actually run here"]

usageError :: [String] -> IO a
usageError ps = do
	usage stderr
	error ("(Unexpected: " ++ show ps)

processCmdLine :: IO CmdLine
processCmdLine = go =<< getArgs
  where
	go ("--check":_) = return Check
	go ("--spin":ps) = case reverse ps of
		(r:"--via":hs) -> Spin
			<$> mapM hostname (reverse hs)
			<*> pure (Just r)
		_ -> Spin <$> mapM hostname ps <*> pure Nothing
	go ("--build":[]) = return Build
	go ("--add-key":k:[]) = return $ AddKey k
	go ("--rm-key":k:[]) = return $ RmKey k
	go ("--set":f:c:[]) = withprivfield f c Set
	go ("--unset":f:c:[]) = withprivfield f c Unset
	go ("--unset-unused":[]) = return UnsetUnused
	go ("--dump":f:c:[]) = withprivfield f c Dump
	go ("--edit":f:c:[]) = withprivfield f c Edit
	go ("--list-fields":[]) = return ListFields
	go ("--merge":[]) = return Merge
	go ("--help":_) = do
		usage stdout
		exitFailure
	go ("--boot":_:[]) = return $ Update Nothing -- for back-compat
	go ("--serialized":s:[]) = serialized Serialized s
	go ("--continue":s:[]) = serialized Continue s
	go ("--gitpush":fin:fout:_) = return $ GitPush (Prelude.read fin) (Prelude.read fout)
	go ("--run":h:[]) = go [h]
	go (h:[])
		| "--" `isPrefixOf` h = usageError [h]
		| otherwise = Run <$> hostname h
	go [] = do
		s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
		if null s
			then errorMessage "Cannot determine hostname! Pass it on the command line."
			else return $ Run s
	go v = usageError v

	withprivfield s c f = case readish s of
		Just pf -> return $ f pf (Context c)
		Nothing -> errorMessage $ "Unknown privdata field " ++ s

	serialized mk s = case readish s of
		Just cmdline -> return $ mk cmdline
		Nothing -> errorMessage $ "serialization failure (" ++ s ++ ")"

data CanRebuild = CanRebuild | NoRebuild

-- | Runs propellor on hosts, as controlled by command-line options.
defaultMain :: [Host] -> IO ()
defaultMain hostlist = withConcurrentOutput $ do
	useFileSystemEncoding
	setupGpgEnv
	Shim.cleanEnv
	checkDebugMode
	cmdline <- processCmdLine
	debug ["command line: ", show cmdline]
	go CanRebuild cmdline
  where
	go cr (Serialized cmdline) = go cr cmdline
	go _ Check = return ()
	go cr Build = buildFirst Nothing cr Build $ return ()
	go _ (Set field context) = setPrivData field context
	go _ (Unset field context) = unsetPrivData field context
	go _ (UnsetUnused) = unsetPrivDataUnused hostlist
	go _ (Dump field context) = dumpPrivData field context
	go _ (Edit field context) = editPrivData field context
	go _ ListFields = listPrivDataFields hostlist
	go _ (AddKey keyid) = addKey keyid
	go _ (RmKey keyid) = rmKey keyid
	go _ c@(ChrootChain _ _ _ _ _) = Chroot.chain hostlist c
	go _ (DockerChain hn cid) = Docker.chain hostlist hn cid
	go _ (DockerInit hn) = Docker.init hn
	go _ (GitPush fin fout) = gitPushHelper fin fout
	go cr (Relay h) = forceConsole >>
		updateFirst Nothing cr (Update (Just h)) (update (Just h))
	go _ (Update Nothing) = forceConsole >>
		fetchFirst (onlyprocess (update Nothing))
	go _ (Update (Just h)) = update (Just h)
	go _ Merge = mergeSpin
	go cr cmdline@(Spin hs mrelay) = buildFirst Nothing cr cmdline $ do
		unless (isJust mrelay) commitSpin
		forM_ hs $ \hn -> withhost hn $ spin mrelay hn
	go cr cmdline@(Run hn) = ifM ((==) 0 <$> getRealUserID)
		( updateFirst (findHost hostlist hn) cr cmdline $ runhost hn
		, fetchFirst $ go cr (Spin [hn] Nothing)
		)
	go cr cmdline@(SimpleRun hn) = forceConsole >>
		fetchFirst (buildFirst (findHost hostlist hn) cr cmdline (runhost hn))
	-- When continuing after a rebuild, don't want to rebuild again.
	go _ (Continue cmdline) = go NoRebuild cmdline

	withhost :: HostName -> (Host -> IO ()) -> IO ()
	withhost hn a = maybe (unknownhost hn hostlist) a (findHost hostlist hn)

	runhost hn = onlyprocess $ withhost hn mainProperties

	onlyprocess = onlyProcess (localdir </> ".lock")

unknownhost :: HostName -> [Host] -> IO a
unknownhost h hosts = errorMessage $ unlines
	[ "Propellor does not know about host: " ++ h
	, "(Perhaps you should specify the real hostname on the command line?)"
	, "(Or, edit propellor's config.hs to configure this host)"
	, "Known hosts: " ++ unwords (map hostName hosts)
	]

-- Builds propellor (when allowed) and if it looks like a new binary,
-- re-execs it to continue.
-- Otherwise, runs the IO action to continue.
--
-- The Host should only be provided when dependencies should be installed
-- as needed to build propellor.
buildFirst :: Maybe Host -> CanRebuild -> CmdLine -> IO () -> IO ()
buildFirst h CanRebuild cmdline next = do
	oldtime <- getmtime
	buildPropellor h
	newtime <- getmtime
	if newtime == oldtime
		then next
		else continueAfterBuild cmdline
  where
	getmtime = catchMaybeIO $ getModificationTime "propellor"
buildFirst _ NoRebuild _ next = next

continueAfterBuild :: CmdLine -> IO a
continueAfterBuild cmdline = go =<< boolSystem "./propellor"
	[ Param "--continue"
	, Param (show cmdline)
	]
  where
	go True = exitSuccess
	go False = exitWith (ExitFailure 1)

fetchFirst :: IO () -> IO ()
fetchFirst next = do
	whenM hasOrigin $
		void fetchOrigin
	next

updateFirst :: Maybe Host -> CanRebuild -> CmdLine -> IO () -> IO ()
updateFirst h canrebuild cmdline next = ifM hasOrigin
	( updateFirst' h canrebuild cmdline next
	, next
	)

-- If changes can be fetched from origin, builds propellor (when allowed)
-- and re-execs the updated propellor binary to continue.
-- Otherwise, runs the IO action to continue.
updateFirst' :: Maybe Host -> CanRebuild -> CmdLine -> IO () -> IO ()
updateFirst' h CanRebuild cmdline next = ifM fetchOrigin
	( do
		buildPropellor h
		continueAfterBuild cmdline
	, next
	)
updateFirst' _ NoRebuild _ next = next

-- Gets the fully qualified domain name, given a string that might be
-- a short name to look up in the DNS.
hostname :: String -> IO HostName
hostname s = go =<< catchDefaultIO [] dnslookup
  where
	dnslookup = getAddrInfo (Just canonname) (Just s) Nothing
	canonname = defaultHints { addrFlags = [AI_CANONNAME] }
	go (AddrInfo { addrCanonName = Just v } : _) = pure v
	go _
		| "." `isInfixOf` s = pure s -- assume it's a fqdn
		| otherwise =
			error $ "cannot find host " ++ s ++ " in the DNS"
