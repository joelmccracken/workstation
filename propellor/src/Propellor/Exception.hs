{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Propellor.Exception where

import Propellor.Types
import Propellor.Types.Exception
import Propellor.Message
import Utility.Exception

import Control.Exception (AsyncException)
#if MIN_VERSION_base(4,7,0)
import Control.Exception (SomeAsyncException)
#endif
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Prelude

-- | Catches all exceptions (except for `StopPropellorException` and
-- `AsyncException` and `SomeAsyncException`) and returns FailedChange.
catchPropellor :: (MonadIO m, MonadCatch m) => m Result -> m Result
catchPropellor a = either err return =<< tryPropellor a
  where
	err e =  warningMessage (show e) >> return FailedChange

catchPropellor' :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchPropellor' a onerr = a `catches`
	[ Handler (\ (e :: AsyncException) -> throwM e)
#if MIN_VERSION_base(4,7,0)
	, Handler (\ (e :: SomeAsyncException) -> throwM e)
#endif
	, Handler (\ (e :: StopPropellorException) -> throwM e)
	, Handler (\ (e :: SomeException) -> onerr e)
	]

-- | Catches all exceptions (except for `StopPropellorException` and
-- `AsyncException`).
tryPropellor :: MonadCatch m => m a -> m (Either SomeException a)
tryPropellor a = (return . Right =<< a) `catchPropellor'` (return . Left)
