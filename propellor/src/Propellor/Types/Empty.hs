module Propellor.Types.Empty where

import qualified Data.Map as M
import qualified Data.Set as S

class Empty t where
	isEmpty :: t -> Bool

instance Empty [a] where
	isEmpty = null

instance Empty (M.Map k v) where
	isEmpty = M.null

instance Empty (S.Set v) where
	isEmpty = S.null
