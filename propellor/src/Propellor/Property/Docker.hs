{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

-- | Maintainer: currently unmaintained; your name here!
--
-- Docker support for propellor
--
-- The existance of a docker container is just another Property of a system,
-- which propellor can set up. See config.hs for an example.

module Propellor.Property.Docker (
	-- * Host properties
	installed,
	configured,
	container,
	docked,
	imageBuilt,
	imagePulled,
	memoryLimited,
	garbageCollected,
	tweaked,
	Image(..),
	latestImage,
	ContainerName,
	Container(..),
	HasImage(..),
	-- * Container configuration
	dns,
	hostname,
	Publishable,
	publish,
	expose,
	user,
	Mountable,
	volume,
	volumes_from,
	workdir,
	memory,
	cpuShares,
	link,
	environment,
	ContainerAlias,
	restartAlways,
	restartOnFailure,
	restartNever,
	-- * Internal use
	init,
	chain,
) where

import Propellor.Base hiding (init)
import Propellor.Types.Docker
import Propellor.Types.Container
import Propellor.Types.Core
import Propellor.Types.CmdLine
import Propellor.Types.Info
import Propellor.Container
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cmd as Cmd
import qualified Propellor.Property.Pacman as Pacman
import qualified Propellor.Shim as Shim
import Utility.Path
import Utility.ThreadScheduler
import Utility.Split

import Control.Concurrent.Async hiding (link)
import System.Posix.Directory
import System.Posix.Process
import Prelude hiding (init)
import Data.List hiding (init)
import qualified Data.Map as M
import System.Console.Concurrent

installed :: Property (DebianLike + ArchLinux)
installed = Apt.installed ["docker.io"] `pickOS` Pacman.installed ["docker"]

-- | Configures docker with an authentication file, so that images can be
-- pushed to index.docker.io. Optional.
configured :: Property (HasInfo + DebianLike)
configured = prop `requires` installed
  where
	prop :: Property (HasInfo + DebianLike)
	prop = withPrivData src anyContext $ \getcfg ->
		property' "docker configured" $ \w -> getcfg $ \cfg -> ensureProperty w $
			"/root/.dockercfg" `File.hasContent` privDataLines cfg
	src = PrivDataSourceFileFromCommand DockerAuthentication
		"/root/.dockercfg" "docker login"

-- | A short descriptive name for a container.
-- Should not contain whitespace or other unusual characters,
-- only [a-zA-Z0-9_-] are allowed
type ContainerName = String

-- | A docker container.
data Container = Container Image Host

instance IsContainer Container where
	containerProperties (Container _ h) = containerProperties h
	containerInfo (Container _ h) = containerInfo h
	setContainerProperties (Container i h) ps = Container i (setContainerProperties h ps)

class HasImage a where
	getImageName :: a -> Image

instance HasImage Image where
	getImageName = id

instance HasImage Container where
	getImageName (Container i _) = i

-- | Defines a Container with a given name, image, and properties.
-- Add properties to configure the Container.
--
-- > container "web-server" (latestImage "debian") $ props
-- >    & publish "80:80"
-- >    & Apt.installed {"apache2"]
-- >    & ...
container :: ContainerName -> Image -> Props metatypes -> Container
container cn image (Props ps) = Container image (Host cn ps info)
  where
	info = dockerInfo mempty <> mconcat (map getInfoRecursive ps)

-- | Ensures that a docker container is set up and running.
--
-- The container has its own Properties which are handled by running
-- propellor inside the container.
--
-- When the container's Properties include DNS info, such as a CNAME,
-- that is propagated to the Info of the Host it's docked in.
--
-- Reverting this property ensures that the container is stopped and
-- removed.
docked :: Container -> RevertableProperty (HasInfo + Linux) (HasInfo + Linux)
docked ctr@(Container _ h) =
	(propagateContainerInfo ctr (go "docked" setup))
		<!>
	(go "undocked" teardown)
  where
	cn = hostName h

	go desc a = property' (desc ++ " " ++ cn) $ \w -> do
		hn <- asks hostName
		let cid = ContainerId hn cn
		ensureProperty w $ a cid (mkContainerInfo cid ctr)

	setup :: ContainerId -> ContainerInfo -> Property Linux
	setup cid (ContainerInfo image runparams) =
		provisionContainer cid
			`requires`
		runningContainer cid image runparams
			`requires`
		installed

	teardown :: ContainerId -> ContainerInfo -> Property Linux
	teardown cid (ContainerInfo image _runparams) =
		combineProperties ("undocked " ++ fromContainerId cid) $ toProps
			[ stoppedContainer cid
			, property ("cleaned up " ++ fromContainerId cid) $
				liftIO $ report <$> mapM id
					[ removeContainer cid
					, removeImage image
					]
			]

-- | Build the image from a directory containing a Dockerfile.
imageBuilt :: HasImage c => FilePath -> c -> Property Linux
imageBuilt directory ctr = built `describe` msg
  where
	msg = "docker image " ++ (imageIdentifier image) ++ " built from " ++ directory
	built :: Property Linux
	built = tightenTargets $
		Cmd.cmdProperty' dockercmd ["build", "--tag", imageIdentifier image, "./"] workDir
			`assume` MadeChange
	workDir p = p { cwd = Just directory }
	image = getImageName ctr

-- | Pull the image from the standard Docker Hub registry.
imagePulled :: HasImage c => c -> Property Linux
imagePulled ctr = pulled `describe` msg
  where
	msg = "docker image " ++ (imageIdentifier image) ++ " pulled"
	pulled :: Property Linux
	pulled = tightenTargets $ 
		Cmd.cmdProperty dockercmd ["pull", imageIdentifier image]
			`assume` MadeChange
	image = getImageName ctr

propagateContainerInfo :: Container -> Property (HasInfo + Linux) -> Property (HasInfo + Linux)
propagateContainerInfo ctr@(Container _ h) p = 
	propagateContainer cn ctr normalContainerInfo $
		p `addInfoProperty` dockerinfo
  where
	dockerinfo = dockerInfo $
		mempty { _dockerContainers = M.singleton cn h }
	cn = hostName h

mkContainerInfo :: ContainerId -> Container -> ContainerInfo
mkContainerInfo cid@(ContainerId hn _cn) (Container img h) =
	ContainerInfo img runparams
  where
	runparams = map (\(DockerRunParam mkparam) -> mkparam hn)
		(_dockerRunParams info)
	info = fromInfo $ hostInfo h'
	h' = setContainerProps h $ containerProps h
		-- Restart by default so container comes up on
		-- boot or when docker is upgraded.
		&^ restartAlways
		-- Expose propellor directory inside the container.
		& volume (localdir++":"++localdir)
		-- Name the container in a predictable way so we
		-- and the user can easily find it later. This property
		-- comes last, so it cannot be overridden.
		& name (fromContainerId cid)

-- | Causes *any* docker images that are not in use by running containers to
-- be deleted. And deletes any containers that propellor has set up
-- before that are not currently running. Does not delete any containers
-- that were not set up using propellor.
--
-- Generally, should come after the properties for the desired containers.
garbageCollected :: Property Linux
garbageCollected = propertyList "docker garbage collected" $ props
	& gccontainers
	& gcimages
  where
	gccontainers :: Property Linux
	gccontainers = property "docker containers garbage collected" $
		liftIO $ report <$> (mapM removeContainer =<< listContainers AllContainers)
	gcimages :: Property Linux
	gcimages = property "docker images garbage collected" $
		liftIO $ report <$> (mapM removeImage =<< listImages)

-- | Tweaks a container to work well with docker.
--
-- Currently, this consists of making pam_loginuid lines optional in
-- the pam config, to work around <https://github.com/docker/docker/issues/5663>
-- which affects docker 1.2.0.
tweaked :: Property Linux
tweaked = tightenTargets $ cmdProperty "sh"
	[ "-c"
	, "sed -ri 's/^session\\s+required\\s+pam_loginuid.so$/session optional pam_loginuid.so/' /etc/pam.d/*"
	]
	`assume` NoChange
	`describe` "tweaked for docker"

-- | Configures the kernel to respect docker memory limits.
--
-- This assumes the system boots using grub 2. And that you don't need any
-- other GRUB_CMDLINE_LINUX_DEFAULT settings.
--
-- Only takes effect after reboot. (Not automated.)
memoryLimited :: Property DebianLike
memoryLimited = tightenTargets $
	"/etc/default/grub" `File.containsLine` cfg
		`describe` "docker memory limited"
		`onChange` (cmdProperty "update-grub" [] `assume` MadeChange)
  where
	cmdline = "cgroup_enable=memory swapaccount=1"
	cfg = "GRUB_CMDLINE_LINUX_DEFAULT=\""++cmdline++"\""

data ContainerInfo = ContainerInfo Image [RunParam]

-- | Parameters to pass to `docker run` when creating a container.
type RunParam = String

-- | ImageID is an image identifier to perform action on images. An
-- ImageID can be the name of an container image, a UID, etc.
--
-- It just encapsulates a String to avoid the definition of a String
-- instance of ImageIdentifier.
newtype ImageID = ImageID String

-- | Used to perform Docker action on an image.
--
-- Minimal complete definition: `imageIdentifier`
class ImageIdentifier i where
	-- | For internal purposes only.
	toImageID :: i -> ImageID
	toImageID = ImageID . imageIdentifier
	-- | A string that Docker can use as an image identifier.
	imageIdentifier :: i -> String

instance ImageIdentifier ImageID where
	imageIdentifier (ImageID i) = i
	toImageID = id

-- | A docker image, that can be used to run a container. The user has
-- to specify a name and can provide an optional tag.
-- See <http://docs.docker.com/userguide/dockerimages/ Docker Image Documention>
-- for more information.
data Image = Image
	{ repository :: String
	, tag :: Maybe String
	}
	deriving (Eq, Read, Show)

-- | Defines a Docker image without any tag. This is considered by
-- Docker as the latest image of the provided repository.
latestImage :: String -> Image
latestImage repo = Image repo Nothing

instance ImageIdentifier Image where
	-- | The format of the imageIdentifier of an `Image` is:
	-- repository | repository:tag
	imageIdentifier i = repository i ++ (maybe "" ((++) ":") $ tag i)

-- | The UID of an image. This UID is generated by Docker.
newtype ImageUID = ImageUID String

instance ImageIdentifier ImageUID where
	imageIdentifier (ImageUID uid) = uid

-- | Set custom dns server for container.
dns :: String -> Property (HasInfo + Linux)
dns = runProp "dns"

-- | Set container host name.
hostname :: String -> Property (HasInfo + Linux)
hostname = runProp "hostname"

-- | Set name of container.
name :: String -> Property (HasInfo + Linux)
name = runProp "name"

class Publishable p where
	toPublish :: p -> String

instance Publishable (Bound Port) where
	toPublish p = val (hostSide p) ++ ":" ++ val (containerSide p)

-- | string format: ip:hostPort:containerPort | ip::containerPort | hostPort:containerPort
instance Publishable String where
	toPublish = id

-- | Publish a container's port to the host
publish :: Publishable p => p -> Property (HasInfo + Linux)
publish = runProp "publish" . toPublish

-- | Expose a container's port without publishing it.
expose :: String -> Property (HasInfo + Linux)
expose = runProp "expose"

-- | Username or UID for container.
user :: String -> Property (HasInfo + Linux)
user = runProp "user"

class Mountable p where
	toMount :: p -> String

instance Mountable (Bound FilePath) where
	toMount p = hostSide p ++ ":" ++ containerSide p

-- | string format: [host-dir]:[container-dir]:[rw|ro]
--
-- With just a directory, creates a volume in the container.
instance Mountable String where
	toMount = id

-- | Mount a volume
volume :: Mountable v => v -> Property (HasInfo + Linux)
volume = runProp "volume" . toMount

-- | Mount a volume from the specified container into the current
-- container.
volumes_from :: ContainerName -> Property (HasInfo + Linux)
volumes_from cn = genProp "volumes-from" $ \hn ->
	fromContainerId (ContainerId hn cn)

-- | Work dir inside the container.
workdir :: String -> Property (HasInfo + Linux)
workdir = runProp "workdir"

-- | Memory limit for container.
-- Format: <number><optional unit>, where unit = b, k, m or g
--
-- Note: Only takes effect when the host has the memoryLimited property
-- enabled.
memory :: String -> Property (HasInfo + Linux)
memory = runProp "memory"

-- | CPU shares (relative weight).
--
-- By default, all containers run at the same priority, but you can tell
-- the kernel to give more CPU time to a container using this property.
cpuShares :: Int -> Property (HasInfo + Linux)
cpuShares = runProp "cpu-shares" . show

-- | Link with another container on the same host.
link :: ContainerName -> ContainerAlias -> Property (HasInfo + Linux)
link linkwith calias = genProp "link" $ \hn ->
	fromContainerId (ContainerId hn linkwith) ++ ":" ++ calias

-- | A short alias for a linked container.
-- Each container has its own alias namespace.
type ContainerAlias = String

-- | This property is enabled by default for docker containers configured by
-- propellor; as well as keeping badly behaved containers running,
-- it ensures that containers get started back up after reboot or
-- after docker is upgraded.
restartAlways :: Property (HasInfo + Linux)
restartAlways = runProp "restart" "always"

-- | Docker will restart the container if it exits nonzero.
-- If a number is provided, it will be restarted only up to that many
-- times.
restartOnFailure :: Maybe Int -> Property (HasInfo + Linux)
restartOnFailure Nothing = runProp "restart" "on-failure"
restartOnFailure (Just n) = runProp "restart" ("on-failure:" ++ show n)

-- | Makes docker not restart a container when it exits
-- Note that this includes not restarting it on boot!
restartNever :: Property (HasInfo + Linux)
restartNever = runProp "restart" "no"

-- | Set environment variable with a tuple composed by the environment
-- variable name and its value.
environment :: (String, String) -> Property (HasInfo + Linux)
environment (k, v) = runProp "env" $ k ++ "=" ++ v

-- | A container is identified by its name, and the host
-- on which it's deployed.
data ContainerId = ContainerId
	{ containerHostName :: HostName
	, containerName :: ContainerName
	}
	deriving (Eq, Read, Show)

-- | Two containers with the same ContainerIdent were started from
-- the same base image (possibly a different version though), and
-- with the same RunParams.
data ContainerIdent = ContainerIdent Image HostName ContainerName [RunParam]
	deriving (Read, Show, Eq)

toContainerId :: String -> Maybe ContainerId
toContainerId s
	| myContainerSuffix `isSuffixOf` s = case separate (== '.') (desuffix s) of
		(cn, hn)
			| null hn || null cn -> Nothing
			| otherwise -> Just $ ContainerId hn cn
	| otherwise = Nothing
  where
	desuffix = reverse . drop len . reverse
	len = length myContainerSuffix

fromContainerId :: ContainerId -> String
fromContainerId (ContainerId hn cn) = cn++"."++hn++myContainerSuffix

myContainerSuffix :: String
myContainerSuffix = ".propellor"

containerDesc :: (IsProp (Property i)) => ContainerId -> Property i -> Property i
containerDesc cid p = p `describe` desc
  where
	desc = "container " ++ fromContainerId cid ++ " " ++ getDesc p

runningContainer :: ContainerId -> Image -> [RunParam] -> Property Linux
runningContainer cid@(ContainerId hn cn) image runps = containerDesc cid $ property "running" $ do
	l <- liftIO $ listContainers RunningContainers
	if cid `elem` l
		then checkident =<< liftIO getrunningident
		else ifM (liftIO $ elem cid <$> listContainers AllContainers)
			( do
				-- The container exists, but is not
				-- running. Its parameters may have
				-- changed, but we cannot tell without
				-- starting it up first.
				void $ liftIO $ startContainer cid
				-- It can take a while for the container to
				-- start up enough for its ident file to be
				-- written, so retry for up to 60 seconds.
				checkident =<< liftIO (retry 60 $ getrunningident)
			, go image
			)
  where
	ident = ContainerIdent image hn cn runps

	-- Check if the ident has changed; if so the
	-- parameters of the container differ and it must
	-- be restarted.
	checkident (Right runningident)
		| runningident == Just ident = noChange
		| otherwise = do
			void $ liftIO $ stopContainer cid
			restartcontainer
	checkident (Left errmsg) = do
		warningMessage errmsg
		return FailedChange

	restartcontainer = do
		oldimage <- liftIO $
			maybe (toImageID image) toImageID <$> commitContainer cid
		void $ liftIO $ removeContainer cid
		go oldimage

	getrunningident = withTmpFile "dockerrunsane" $ \t h -> do
		-- detect #774376 which caused docker exec to not enter
		-- the container namespace, and be able to access files
		-- outside
		hClose h
		void . checkSuccessProcess . processHandle =<<
			createProcess (inContainerProcess cid []
				["rm", "-f", t])
		ifM (doesFileExist t)
			( Right . readish <$>
				readProcess' (inContainerProcess cid []
					["cat", propellorIdent])
			, return $ Left "docker exec failed to enter chroot properly (maybe an old kernel version?)"
			)

	retry :: Int -> IO (Either e (Maybe a)) -> IO (Either e (Maybe a))
	retry 0 _ = return (Right Nothing)
	retry n a = do
		v <- a
		case v of
			Right Nothing -> do
				threadDelaySeconds (Seconds 1)
				retry (n-1) a
			_ -> return v

	go :: ImageIdentifier i => i -> Propellor Result
	go img = liftIO $ do
		clearProvisionedFlag cid
		createDirectoryIfMissing True (takeDirectory $ identFile cid)
		shim <- Shim.setup (localdir </> "propellor") Nothing (localdir </> shimdir cid)
		writeFile (identFile cid) (show ident)
		toResult <$> runContainer img
			(runps ++ ["-i", "-d", "-t"])
			[shim, "--continue", show (DockerInit (fromContainerId cid))]

-- | Called when propellor is running inside a docker container.
-- The string should be the container's ContainerId.
--
-- This process is effectively init inside the container.
-- It even needs to wait on zombie processes!
--
-- In the foreground, run an interactive bash (or sh) shell,
-- so that the user can interact with it when attached to the container.
--
-- When the system reboots, docker restarts the container, and this is run
-- again. So, to make the necessary services get started on boot, this needs
-- to provision the container then. However, if the container is already
-- being provisioned by the calling propellor, it would be redundant and
-- problimatic to also provisoon it here, when not booting up.
--
-- The solution is a flag file. If the flag file exists, then the container
-- was already provisioned. So, it must be a reboot, and time to provision
-- again. If the flag file doesn't exist, don't provision here.
init :: String -> IO ()
init s = case toContainerId s of
	Nothing -> error $ "Invalid ContainerId: " ++ s
	Just cid -> do
		changeWorkingDirectory localdir
		writeFile propellorIdent . show =<< readIdentFile cid
		whenM (checkProvisionedFlag cid) $ do
			let shim = Shim.file (localdir </> "propellor") (localdir </> shimdir cid)
			unlessM (boolSystem shim [Param "--continue", Param $ show $ toChain cid]) $
				warningMessage "Boot provision failed!"
		void $ async $ job reapzombies
		job $ do
			flushConcurrentOutput
			void $ tryIO $ ifM (inPath "bash")
				( boolSystem "bash" [Param "-l"]
				, boolSystem "/bin/sh" []
				)
			putStrLn "Container is still running. Press ^P^Q to detach."
  where
	job = forever . void . tryIO
	reapzombies = void $ getAnyProcessStatus True False

-- | Once a container is running, propellor can be run inside
-- it to provision it.
provisionContainer :: ContainerId -> Property Linux
provisionContainer cid = containerDesc cid $ property "provisioned" $ liftIO $ do
	let shim = Shim.file (localdir </> "propellor") (localdir </> shimdir cid)
	let params = ["--continue", show $ toChain cid]
	msgh <- getMessageHandle
	let p = inContainerProcess cid
		(if isConsole msgh then ["-it"] else [])
		(shim : params)
	r <- chainPropellor p
	when (r /= FailedChange) $
		setProvisionedFlag cid
	return r

toChain :: ContainerId -> CmdLine
toChain cid = DockerChain (containerHostName cid) (fromContainerId cid)

chain :: [Host] -> HostName -> String -> IO ()
chain hostlist hn s = case toContainerId s of
	Nothing -> errorMessage "bad container id"
	Just cid -> case findHostNoAlias hostlist hn of
		Nothing -> errorMessage ("cannot find host " ++ hn)
		Just parenthost -> case M.lookup (containerName cid) (_dockerContainers $ fromInfo $ hostInfo parenthost) of
			Nothing -> errorMessage ("cannot find container " ++ containerName cid ++ " docked on host " ++ hn)
			Just h -> go cid h
  where
	go cid h = do
		changeWorkingDirectory localdir
		onlyProcess (provisioningLock cid) $
			runChainPropellor (setcaps h) $ 
				ensureChildProperties $ hostProperties h
	setcaps h = h { hostInfo = hostInfo h `addInfo` [HostnameContained, FilesystemContained] }

stopContainer :: ContainerId -> IO Bool
stopContainer cid = boolSystem dockercmd [Param "stop", Param $ fromContainerId cid ]

startContainer :: ContainerId -> IO Bool
startContainer cid = boolSystem dockercmd [Param "start", Param $ fromContainerId cid ]

stoppedContainer :: ContainerId -> Property Linux
stoppedContainer cid = containerDesc cid $ property' desc $ \w ->
	ifM (liftIO $ elem cid <$> listContainers RunningContainers)
		( liftIO cleanup `after` ensureProperty w stop
		, return NoChange
		)
  where
	desc = "stopped"
	stop :: Property Linux
	stop = property desc $ liftIO $ toResult <$> stopContainer cid
	cleanup = do
		nukeFile $ identFile cid
		removeDirectoryRecursive $ shimdir cid
		clearProvisionedFlag cid

removeContainer :: ContainerId -> IO Bool
removeContainer cid = catchBoolIO $
	snd <$> processTranscript dockercmd ["rm", fromContainerId cid ] Nothing

removeImage :: ImageIdentifier i => i -> IO Bool
removeImage image = catchBoolIO $
	snd <$> processTranscript dockercmd ["rmi", imageIdentifier image] Nothing

runContainer :: ImageIdentifier i => i -> [RunParam] -> [String] -> IO Bool
runContainer image ps cmd = boolSystem dockercmd $ map Param $
	"run" : (ps ++ (imageIdentifier image) : cmd)

inContainerProcess :: ContainerId -> [String] -> [String] -> CreateProcess
inContainerProcess cid ps cmd = proc dockercmd ("exec" : ps ++ [fromContainerId cid] ++ cmd)

commitContainer :: ContainerId -> IO (Maybe ImageUID)
commitContainer cid = catchMaybeIO $
	ImageUID . takeWhile (/= '\n')
		<$> readProcess dockercmd ["commit", fromContainerId cid]

data ContainerFilter = RunningContainers | AllContainers
	deriving (Eq)

-- | Only lists propellor managed containers.
listContainers :: ContainerFilter -> IO [ContainerId]
listContainers status =
	mapMaybe toContainerId . concatMap (split ",")
		. mapMaybe (lastMaybe . words) . lines
		<$> readProcess dockercmd ps
  where
	ps
		| status == AllContainers = baseps ++ ["--all"]
		| otherwise = baseps
	baseps = ["ps", "--no-trunc"]

listImages :: IO [ImageUID]
listImages = map ImageUID . lines <$> readProcess dockercmd ["images", "--all", "--quiet"]

runProp :: String -> RunParam -> Property (HasInfo + Linux)
runProp field v = tightenTargets $ pureInfoProperty (param) $
	mempty { _dockerRunParams = [DockerRunParam (\_ -> "--"++param)] }
  where
	param = field++"="++v

genProp :: String -> (HostName -> RunParam) -> Property (HasInfo + Linux)
genProp field mkval = tightenTargets $ pureInfoProperty field $
	mempty { _dockerRunParams = [DockerRunParam (\hn -> "--"++field++"=" ++ mkval hn)] }

dockerInfo :: DockerInfo -> Info
dockerInfo i = mempty `addInfo` i

-- | The ContainerIdent of a container is written to
-- </.propellor-ident> inside it. This can be checked to see if
-- the container has the same ident later.
propellorIdent :: FilePath
propellorIdent = "/.propellor-ident"

provisionedFlag :: ContainerId -> FilePath
provisionedFlag cid = "docker" </> fromContainerId cid ++ ".provisioned"

clearProvisionedFlag :: ContainerId -> IO ()
clearProvisionedFlag = nukeFile . provisionedFlag

setProvisionedFlag :: ContainerId -> IO ()
setProvisionedFlag cid = do
	createDirectoryIfMissing True (takeDirectory (provisionedFlag cid))
	writeFile (provisionedFlag cid) "1"

checkProvisionedFlag :: ContainerId -> IO Bool
checkProvisionedFlag = doesFileExist . provisionedFlag

provisioningLock :: ContainerId -> FilePath
provisioningLock cid = "docker" </> fromContainerId cid ++ ".lock"

shimdir :: ContainerId -> FilePath
shimdir cid = "docker" </> fromContainerId cid ++ ".shim"

identFile :: ContainerId -> FilePath
identFile cid = "docker" </> fromContainerId cid ++ ".ident"

readIdentFile :: ContainerId -> IO ContainerIdent
readIdentFile cid = fromMaybe (error "bad ident in identFile")
	. readish <$> readFile (identFile cid)

dockercmd :: String
dockercmd = "docker"

report :: [Bool] -> Result
report rmed
	| or rmed = MadeChange
	| otherwise = NoChange

