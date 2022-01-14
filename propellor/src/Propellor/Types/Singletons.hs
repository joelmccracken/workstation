{-# LANGUAGE CPP, DataKinds, PolyKinds, TypeOperators, TypeFamilies, GADTs, FlexibleContexts #-}

-- | Simple implementation of singletons, portable back to ghc 7.6.3

module Propellor.Types.Singletons (
	module Propellor.Types.Singletons,
	KProxy(..)
) where

#if __GLASGOW_HASKELL__ > 707
import Data.Proxy (KProxy(..))
#else
data KProxy (a :: *) = KProxy
#endif

-- | The data family of singleton types.
data family Sing (x :: k)

-- | A class used to pass singleton values implicitly.
class SingI t where
	sing :: Sing t

-- Lists of singletons
data instance Sing (x :: [k]) where
	Nil :: Sing '[]
	Cons :: Sing x -> Sing xs -> Sing (x ': xs)
instance (SingI x, SingI xs) => SingI (x ': xs) where sing = Cons sing sing
instance SingI '[] where sing = Nil

data instance Sing (x :: Bool) where
	TrueS :: Sing 'True
	FalseS :: Sing 'False
instance SingI 'True where sing = TrueS
instance SingI 'False where sing = FalseS

class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
	type DemoteRep kparam :: *
	-- | From singleton to value.
	fromSing :: Sing (a :: k) -> DemoteRep kparam

instance SingKind ('KProxy :: KProxy a) => SingKind ('KProxy :: KProxy [a]) where
	type DemoteRep ('KProxy :: KProxy [a]) = [DemoteRep ('KProxy :: KProxy a)]
	fromSing Nil = []
	fromSing (Cons x xs) = fromSing x : fromSing xs

instance SingKind ('KProxy :: KProxy Bool) where
	type DemoteRep ('KProxy :: KProxy Bool) = Bool
	fromSing FalseS = False
	fromSing TrueS = True
