{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types.Chroot where

import Propellor.Types
import Propellor.Types.Empty
import Propellor.Types.Info

import qualified Data.Semigroup as Sem
import qualified Data.Map as M
import Data.Monoid
import Prelude

data ChrootInfo = ChrootInfo
	{ _chroots :: M.Map FilePath Host
	, _chrootCfg :: ChrootCfg
	}
	deriving (Show, Typeable)

instance IsInfo ChrootInfo where
	propagateInfo _ = PropagateInfo False

instance Sem.Semigroup ChrootInfo where
	old <> new = ChrootInfo
		{ _chroots = M.union (_chroots old) (_chroots new)
		, _chrootCfg = _chrootCfg old <> _chrootCfg new
		}

instance Monoid ChrootInfo where
	mempty = ChrootInfo mempty mempty
	mappend = (Sem.<>)

instance Empty ChrootInfo where
	isEmpty i = and
		[ isEmpty (_chroots i)
		, isEmpty (_chrootCfg i)
		]

data ChrootCfg
	= NoChrootCfg
	| SystemdNspawnCfg [(String, Bool)]
	deriving (Show, Eq)

instance Sem.Semigroup ChrootCfg where
	v <> NoChrootCfg = v
	NoChrootCfg <> v = v
	SystemdNspawnCfg l1 <> SystemdNspawnCfg l2 =
		SystemdNspawnCfg (l1 <> l2)

instance Monoid ChrootCfg where
	mempty = NoChrootCfg
	mappend = (Sem.<>)

instance Empty ChrootCfg where
	isEmpty c= c == NoChrootCfg
