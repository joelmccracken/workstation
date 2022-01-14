{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies #-}

-- | This module adds conductors to propellor. A conductor is a Host that
-- is responsible for running propellor on other hosts
--
-- This eliminates the need to manually run propellor --spin to
-- update the conducted hosts, and can be used to orchestrate updates
-- to hosts.
--
-- The conductor needs to be able to ssh to the hosts it conducts,
-- and run propellor, as root. To this end, 
-- the `Propellor.Property.Ssh.knownHost` property is automatically
-- added to the conductor, so it knows the host keys of the relevant hosts.
-- Also, each conducted host is configured to let its conductor
-- ssh in as root, by automatically adding the
-- `Propellor.Property.Ssh.authorizedKeysFrom` property.
--
-- It's left up to you to use `Propellor.Property.Ssh.userKeys` to
-- configure the ssh keys for the root user on conductor hosts,
-- and to use `Ssh.hostKeys` to configure the host keys for the 
-- conducted hosts.
--
-- For example, if you have some webservers and a dnsserver,
-- and want the master host to conduct all of them:
--
-- > import Propellor
-- > import Propellor.Property.Conductor
-- > import qualified Propellor.Property.Ssh as Ssh
-- > import qualified Propellor.Property.Cron as Cron
-- > 
-- > main = defaultMain (orchestrate hosts)
-- >
-- > hosts =
-- > 	[ master
-- >	, dnsserver
-- >	] ++ webservers
-- > 
-- > dnsserver = host "dns.example.com"
-- >	& Ssh.hostKeys hostContext [(SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB3BJ2GqZiTR2LEoDXyYFgh/BduWefjdKXAsAtzS9zeI")]
-- >	& ...
-- > 
-- > webservers =
-- >    [ host "www1.example.com"
-- >		& Ssh.hostKeys hostContext [(SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICfFntnesZcYz2B2T41ay45igfckXRSh5uVffkuCQkLv")]
-- >		& ...
-- >	, ...
-- >	]
-- >
-- > master = host "master.example.com"
-- >	& Ssh.userKeys (User "root") [(SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFWD0Hau5FDLeNrDHKilNMKm9c68R3WD+NJOp2jPWvJV")]
-- >	& conducts webservers
-- >		`before` conducts dnsserver
-- >	& Cron.runPropellor
--
-- Notice that, in the above example, the the webservers are conducted
-- first. Only once the webservers have successfully been set up is the
-- dnsserver updated. This way, when adding a new web server, the dns
-- won't list it until it's ready.
--
-- There can be multiple conductors, and conductors can conduct other
-- conductors if you need such a hierarchy. (Loops in the hierarchy, such
-- as a host conducting itself, are detected and automatically broken.)
--
-- While it's allowed for a single host to be conducted by
-- multiple conductors, the results can be discordent.
-- Since only one propellor process can be run on a host at a time,
-- one of the conductors will fail to communicate with it.
--
-- Note that a conductor can see all PrivData of the hosts it conducts.

module Propellor.Property.Conductor (
	orchestrate,
	Conductable(..),
) where

import Propellor.Base
import Propellor.Container
import Propellor.Spin (spin')
import Propellor.PrivData.Paths
import Propellor.Types.Info
import qualified Propellor.Property.Ssh as Ssh

import qualified Data.Set as S
import qualified Data.Semigroup as Sem

-- | Class of things that can be conducted.
--
-- There are instances for single hosts, and for lists of hosts.
-- With a list, each listed host will be conducted in turn. Failure to conduct
-- one host does not prevent conducting subsequent hosts in the list, but
-- will be propagated as an overall failure of the property.
class Conductable c where
	conducts :: c -> RevertableProperty (HasInfo + UnixLike) (HasInfo + UnixLike)

instance Conductable Host where
	conducts h = conductorFor h <!> notConductorFor h

instance Conductable [Host] where
	conducts hs = 
		propertyList desc (toProps $ map (setupRevertableProperty . conducts) hs)
			<!>
		propertyList desc (toProps $ map (undoRevertableProperty . conducts) hs)
	  where
		desc = cdesc $ unwords $ map hostName hs

data Orchestra
	= Conductor Host [Orchestra]
	| Conducted Host

instance Show Orchestra where
	show (Conductor h l) = "Conductor " ++ hostName h ++ " (" ++ show l ++ ")"
	show (Conducted h) = "Conducted " ++ hostName h

fullOrchestra :: Orchestra -> Bool
fullOrchestra (Conductor _ _) = True
fullOrchestra (Conducted _) = False

topHost :: Orchestra -> Host
topHost (Conducted h) = h
topHost (Conductor h _) = h

allHosts :: Orchestra -> [Host]
allHosts (Conducted h) = [h]
allHosts (Conductor h l) = h : concatMap allHosts l

-- Makes an Orchestra for the host, and any hosts it's conducting.
mkOrchestra :: Host -> Orchestra
mkOrchestra = fromJust . go S.empty
  where
	go seen h
		| S.member (hostName h) seen = Nothing -- break loop
		| otherwise = Just $ case fromInfo (hostInfo h) of
			ConductorFor [] -> Conducted h
			ConductorFor l -> 
				let seen' = S.insert (hostName h) seen
				in Conductor h (mapMaybe (go seen') l)

-- Combines the two orchestras, if there's a place, or places where they
-- can be grafted together.
combineOrchestras :: Orchestra -> Orchestra -> Maybe Orchestra
combineOrchestras a b = combineOrchestras' a b <|> combineOrchestras' b a

combineOrchestras' :: Orchestra -> Orchestra -> Maybe Orchestra
combineOrchestras' (Conducted h) b
	| sameHost h (topHost b) = Just b
	| otherwise = Nothing
combineOrchestras' (Conductor h os) (Conductor h' os')
	| sameHost h h' = Just $ Conductor h (concatMap combineos os')
  where
	combineos o = case mapMaybe (`combineOrchestras` o) os of
		[] -> [o]
		os'' -> os''
combineOrchestras' a@(Conductor h _) (Conducted h')
	| sameHost h h' = Just a
combineOrchestras' (Conductor h os) b
	| null (catMaybes (map snd osgrafts)) = Nothing
	| otherwise = Just $ Conductor h (map (uncurry fromMaybe) osgrafts)
  where
	osgrafts = zip os (map (`combineOrchestras` b) os)

sameHost :: Host -> Host -> Bool
sameHost a b = hostName a == hostName b

-- Removes any loops that may be present in the Orchestra involving
-- the passed Host. This is a matter of traversing the Orchestra
-- top-down, and removing all occurrances of the host after the first
-- one seen.
deloop :: Host -> Orchestra -> Orchestra
deloop _ (Conducted h) = Conducted h
deloop thehost (Conductor htop ostop) = Conductor htop $
	fst $ seekh [] ostop (sameHost htop thehost)
  where
	seekh l [] seen = (l, seen)
	seekh l ((Conducted h) : rest) seen
		| sameHost h thehost = 
			if seen
				then seekh l rest seen
				else seekh (Conducted h : l) rest True
		| otherwise = seekh (Conducted h:l) rest seen
	seekh l ((Conductor h os) : rest) seen
		| sameHost h thehost =
			if seen
				then seekh l rest seen
				else 
					let (os', _seen') = seekh [] os True
					in seekh (Conductor h os' : l) rest True
		| otherwise = 
			let (os', seen') = seekh [] os seen
			in seekh (Conductor h os' : l) rest seen'

-- Extracts the Orchestras from a list of hosts.
--
-- Method: For each host that is a conductor, check the
-- list of orchesteras to see if any already contain that host, or
-- any of the hosts it conducts. If so, add the host to that
-- orchestra. If not, start a new orchestra.
--
-- The result is a set of orchestras, which are each fully disconnected
-- from the other. Some may contain loops.
extractOrchestras :: [Host] -> [Orchestra]
extractOrchestras = filter fullOrchestra . go [] . map mkOrchestra
  where
	go os [] = os
	go os (o:rest) = 
		let os' = zip os (map (combineOrchestras o) os)
		in case catMaybes (map snd os') of
			[] -> go (o:os) rest
			[_] -> go (map (uncurry fromMaybe) os') rest
			_ -> error "Bug: Host somehow ended up in multiple Orchestras!"

-- | Pass this a list of all your hosts; it will finish setting up
-- orchestration as configured by the `conducts` properties you add to
-- hosts.
--
-- > main = defaultMain $ orchestrate hosts
orchestrate :: [Host] -> [Host]
orchestrate hs = map go hs
  where
	go h
		| isOrchestrated (fromInfo (hostInfo h)) = h
		| otherwise = foldl orchestrate' (removeold h) (map (deloop h) os)
	os = extractOrchestras hs

	removeold h = foldl removeold' h (oldconductorsof h)
	removeold' h oldconductor = setContainerProps h $ containerProps h
		! conductedBy oldconductor

	oldconductors = zip hs (map (fromInfo . hostInfo) hs)
	oldconductorsof h = flip mapMaybe oldconductors $ 
		\(oldconductor, NotConductorFor l) ->
			if any (sameHost h) l
				then Just oldconductor
				else Nothing

orchestrate' :: Host -> Orchestra -> Host
orchestrate' h (Conducted _) = h
orchestrate' h (Conductor c l)
	| sameHost h c = cont $ addConductorPrivData h (concatMap allHosts l)
	| any (sameHost h) (map topHost l) = cont $
		setContainerProps h $ containerProps h
			& conductedBy c
	| otherwise = cont h
  where
	cont h' = foldl orchestrate' h' l

-- The host this property is added to becomes the conductor for the
-- specified Host. Note that `orchestrate` must be used for this property
-- to have any effect.
conductorFor :: Host -> Property (HasInfo + UnixLike)
conductorFor h = go
	`setInfoProperty` (toInfo (ConductorFor [h]))
	`requires` setupRevertableProperty (conductorKnownHost h)
	`requires` Ssh.installed
  where
	desc = cdesc (hostName h)

	go :: Property UnixLike
	go = property desc $ ifM (isOrchestrated <$> askInfo)
		( do
			pm <- liftIO $ filterPrivData h
				<$> readPrivDataFile privDataLocal
			liftIO $ spin' (Just pm) Nothing (hostName h) h
			-- Don't know if the spin made a change to
			-- the remote host or not, but in any case,
			-- the local host was not changed.
			noChange
		, do
			warningMessage "Can't conduct; either orchestrate has not been used, or there is a conductor loop."
			return FailedChange
		)

-- Reverts conductorFor.
notConductorFor :: Host -> Property (HasInfo + UnixLike)
notConductorFor h = (doNothing :: Property UnixLike)
	`setInfoProperty` (toInfo (NotConductorFor [h]))
	`describe` desc
	`requires` undoRevertableProperty (conductorKnownHost h)
  where
	desc = "not " ++ cdesc (hostName h)

conductorKnownHost :: Host -> RevertableProperty UnixLike UnixLike
conductorKnownHost h = 
	mk Ssh.knownHost
		<!>
	mk Ssh.unknownHost
  where
	mk p = p [h] (hostName h) (User "root")

-- Gives a conductor access to all the PrivData of the specified hosts.
-- This allows it to send it on the the hosts when conducting it.
--
-- This is not done in conductorFor, so that it can be added
-- at the orchestration stage, and so is not added when there's a loop.
addConductorPrivData :: Host -> [Host] -> Host
addConductorPrivData h hs = h { hostInfo = hostInfo h <> i }
  where
	i = mempty 
		`addInfo` mconcat (map privinfo hs)
		`addInfo` Orchestrated (Any True)
	privinfo h' = forceHostContext (hostName h') $ fromInfo (hostInfo h')

-- Use this property to let the specified conductor ssh in and run propellor.
conductedBy :: Host -> RevertableProperty UnixLike UnixLike
conductedBy h = (setup <!> teardown)
	`describe` ("conducted by " ++ hostName h)
  where
	setup = User "root" `Ssh.authorizedKeysFrom` (User "root", h)
		`requires` Ssh.installed
	teardown = User "root" `Ssh.unauthorizedKeysFrom` (User "root", h)

cdesc :: String -> Desc
cdesc n = "conducting " ++ n

-- A Host's Info indicates when it's a conductor for hosts, and when it's
-- stopped being a conductor.
newtype ConductorFor = ConductorFor [Host]
	deriving (Typeable, Sem.Semigroup, Monoid)
newtype NotConductorFor = NotConductorFor [Host]
	deriving (Typeable, Sem.Semigroup, Monoid)

instance Show ConductorFor where
	show (ConductorFor l) = "ConductorFor " ++ show (map hostName l)
instance Show NotConductorFor where
	show (NotConductorFor l) = "NotConductorFor " ++ show (map hostName l)

instance IsInfo ConductorFor where
	propagateInfo _ = PropagateInfo False
instance IsInfo NotConductorFor where
	propagateInfo _ = PropagateInfo False

-- Added to Info when a host has been orchestrated.
newtype Orchestrated = Orchestrated Any
	deriving (Typeable, Sem.Semigroup, Monoid, Show)
instance IsInfo Orchestrated where
	propagateInfo _ = PropagateInfo False

isOrchestrated :: Orchestrated -> Bool
isOrchestrated (Orchestrated v) = getAny v
