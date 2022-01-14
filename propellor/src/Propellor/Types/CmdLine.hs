module Propellor.Types.CmdLine where

import Propellor.Types.OS
import Propellor.Types.PrivData
import Propellor.Types.Container

import System.Posix.Types

-- | All the command line actions that propellor can perform.
data CmdLine
	= Run HostName
	| Spin [HostName] (Maybe HostName)
	| SimpleRun HostName
	| Set PrivDataField Context
	| Unset PrivDataField Context
	| UnsetUnused
	| Dump PrivDataField Context
	| Edit PrivDataField Context
	| ListFields
	| AddKey String
	| RmKey String
	| Merge
	| Serialized CmdLine
	| Continue CmdLine
	| Update (Maybe HostName)
	| Relay HostName
	| DockerInit HostName
	| DockerChain HostName String
	| ChrootChain HostName FilePath Bool Bool [ContainerCapability]
	| GitPush Fd Fd
	| Check
	| Build
	deriving (Read, Show, Eq)
