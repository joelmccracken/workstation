{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Propellor.Types.Container where

import Propellor.Types.Info

-- | A value that can be bound between the host and a container.
--
-- For example, a Bound Port is a Port on the container that is bound to
-- a Port on the host.
data Bound v = Bound
	{ hostSide :: v
	, containerSide :: v
	}

-- | Create a Bound value, from two different values for the host and
-- container.
--
-- For example, @Port 8080 -<- Port 80@ means that port 8080 on the host
-- is bound to port 80 from the container.
(-<-) :: (hostv ~ v, containerv ~ v) => hostv -> containerv -> Bound v
(-<-) = Bound

-- | Flipped version of -<- with the container value first and host value
-- second.
(->-) :: (containerv ~ v, hostv ~ v) => containerv -> hostv -> Bound v
(->-) = flip (-<-)

-- | Create a Bound value, that is the same on both the host and container.
same :: v -> Bound v
same v = Bound v v

-- | Capabilities of a container.
data ContainerCapability
	= HostnameContained
	-- ^ The container has its own hostname (and domain name)
	-- separate from the system that contains it.
	| FilesystemContained
	-- ^ The container has its own root filesystem, rather than sharing
	-- the root filesystem of the system that contains it.
	deriving (Typeable, Eq, Read, Show)

-- | A [ContainerCapability] can be used as Info.
-- It does not propagate out to the Host.
-- When not in a container, the Info value will be [].
instance IsInfo [ContainerCapability] where
        propagateInfo _ = PropagateInfo False
