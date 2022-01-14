{-# LANGUAGE DataKinds, TypeFamilies #-}

module Propellor.Container where

import Propellor.Types
import Propellor.Types.Core
import Propellor.Types.MetaTypes
import Propellor.Types.Info
import Propellor.Info
import Propellor.PrivData
import Propellor.PropAccum

class IsContainer c where
	containerProperties :: c -> [ChildProperty]
	containerInfo :: c -> Info
	setContainerProperties :: c -> [ChildProperty] -> c

instance IsContainer Host where
	containerProperties = hostProperties
	containerInfo = hostInfo
	setContainerProperties h ps = host (hostName h) (Props ps)

-- | Note that the metatype of a container's properties is not retained,
-- so this defaults to UnixLike. So, using this with setContainerProps can
-- add properties to a container that conflict with properties already in it.
-- Use caution when using this; only add properties that do not have
-- restricted targets.
containerProps :: IsContainer c => c -> Props UnixLike
containerProps = Props . containerProperties

setContainerProps :: IsContainer c => c -> Props metatypes -> c
setContainerProps c (Props ps) = setContainerProperties c ps

-- | Adjust the provided Property, adding to its
-- propertyChidren the properties of the provided container.
-- 
-- The Info of the propertyChildren is adjusted to only include 
-- info that should be propagated out to the Property.
--
-- Any PrivInfo that uses HostContext is adjusted to use the name
-- of the container as its context.
propagateContainer
	::
		-- Since the children being added probably have info,
		-- require the Property's metatypes to have info.
		-- -Wredundant-constraints is turned off because
		-- this constraint appears redundant, but is actually
		-- crucial.
		( IncludesInfo metatypes ~ 'True
		, IsContainer c
		)
	=> String
	-> c
	-> (PropagateInfo -> Bool)
	-> Property metatypes
	-> Property metatypes
propagateContainer containername c wanted prop = prop
	`addChildren` map convert (containerProperties c)
  where
	convert p = 
		let n = property'' (getDesc p) (getSatisfy p) :: Property UnixLike
		    n' = n
		    	`setInfoProperty` mapInfo (forceHostContext containername)
				(propagatableInfo wanted (getInfo p))
		   	`addChildren` map convert (getChildren p)
		in toChildProperty n'

-- | Filters out parts of the Info that should not propagate out of a
-- container.
propagatableInfo :: (PropagateInfo -> Bool) -> Info -> Info
propagatableInfo wanted (Info l) = Info $
	filter (\(InfoEntry a) -> wanted (propagateInfo a)) l

normalContainerInfo :: PropagateInfo -> Bool
normalContainerInfo PropagatePrivData = True
normalContainerInfo (PropagateInfo b) = b

onlyPrivData :: PropagateInfo -> Bool
onlyPrivData PropagatePrivData = True
onlyPrivData (PropagateInfo _) = False
