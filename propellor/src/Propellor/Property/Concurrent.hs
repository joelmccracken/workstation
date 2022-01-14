{-# LANGUAGE FlexibleContexts #-}

-- | Propellor properties can be made to run concurrently, using this
-- module. This can speed up propellor, at the expense of using more CPUs
-- and other resources.
--
-- It's up to you to make sure that properties that you make run concurrently
-- don't implicitly depend on one-another. The worst that can happen
-- though, is that propellor fails to ensure some of the properties,
-- and tells you what went wrong.
--
-- Another potential problem is that output of concurrent properties could
-- interleave into a scrambled mess. This is mostly prevented; all messages
-- output by propellor are concurrency safe, including `errorMessage`,
-- `infoMessage`, etc. However, if you write a property that directly
-- uses `print` or `putStrLn`, you can still experience this problem.
--
-- Similarly, when properties run external commands, the command's output
-- can be a problem for concurrency. No need to worry;
-- `Propellor.Property.Cmd.createProcess` is concurrent output safe
-- (it actually uses `Propellor.Message.createProcessConcurrent`), and
-- everything else in propellor that runs external commands is built on top
-- of that. Of course, if you import System.Process and use it in a
-- property, you can bypass that and shoot yourself in the foot.
--
-- Finally, anything that directly accesses the tty can bypass
-- these protections. That's sometimes done for eg, password prompts.
-- A well-written property should avoid running interactive commands
-- anyway.

module Propellor.Property.Concurrent (
	concurrently,
	concurrentList,
	props,
	getNumProcessors,
	concurrentSatisfy,
) where

import Propellor.Base
import Propellor.Types.Core
import Propellor.Types.MetaTypes

import Control.Concurrent
import qualified Control.Concurrent.Async as A
import GHC.Conc (getNumProcessors)
import Control.Monad.RWS.Strict

-- | Ensures two properties concurrently.
--
-- >	& foo `concurrently` bar
--
-- To ensure three properties concurrently, just use this combinator twice:
--
-- >	& foo `concurrently` bar `concurrently` baz
concurrently
	:: (IsProp p1, IsProp p2, Combines p1 p2, IsProp (CombinedType p1 p2))
	=> p1
	-> p2
	-> CombinedType p1 p2
concurrently p1 p2 = (combineWith go go p1 p2)
	`describe` d
  where
	d = getDesc p1 ++ " `concurrently` " ++ getDesc p2
	-- Increase the number of capabilities right up to the number of
	-- processors, so that A `concurrently` B `concurrently` C
	-- runs all 3 properties on different processors when possible.
	go (Just a1) (Just a2) = Just $ do
		n <- liftIO getNumProcessors
		withCapabilities n $
			concurrentSatisfy a1 a2
	go (Just a1) Nothing = Just a1
	go Nothing (Just a2) = Just a2
	go Nothing Nothing = Nothing

-- | Ensures all the properties in the list, with a specified amount of
-- concurrency.
-- 
-- > concurrentList (pure 2) "demo" $ props
-- >	& foo
-- >	& bar
-- >	& baz
--
-- The above example will run foo and bar concurrently, and once either of
-- those 2 properties finishes, will start running baz.
concurrentList :: SingI metatypes => IO Int -> Desc -> Props (MetaTypes metatypes) -> Property (MetaTypes metatypes)
concurrentList getn d (Props ps) = property d go `addChildren` ps
  where
	go = do
		n <- liftIO getn
		withCapabilities n $
			startworkers n =<< liftIO (newMVar ps)
	startworkers n q
		| n < 1 = return NoChange
		| n == 1 = worker q NoChange
		| otherwise = 
			worker q NoChange
				`concurrentSatisfy`
			startworkers (n-1) q
	worker q r = do
		v <- liftIO $ modifyMVar q $ \v -> case v of
			[] -> return ([], Nothing)
			(p:rest) -> return (rest, Just p)
		case v of
			Nothing -> return r
			Just p -> do
				hn <- asks hostName
				r' <- case getSatisfy p of
					Nothing -> return NoChange
					Just a -> actionMessageOn hn (getDesc p) a
				worker q (r <> r')

-- | Run an action with the number of capabiities increased as necessary to
-- allow running on the specified number of cores.
--
-- Never increases the number of capabilities higher than the actual number
-- of processors.
withCapabilities :: Int -> Propellor a -> Propellor a
withCapabilities n a = bracket setup cleanup (const a)
  where
	setup = do
		np <- liftIO getNumProcessors
		let n' = min n np
		c <- liftIO getNumCapabilities
		when (n' > c) $ 
			liftIO $ setNumCapabilities n'
		return c
	cleanup = liftIO . setNumCapabilities

-- | Running Propellor actions concurrently.
concurrentSatisfy :: Propellor Result -> Propellor Result -> Propellor Result
concurrentSatisfy a1 a2 = do
	h <- ask
	((r1, w1), (r2, w2)) <- liftIO $
		runp a1 h `A.concurrently` runp a2 h
	tell (w1 <> w2)
	return (r1 <> r2)
  where
	runp a h = evalRWST (runWithHost (catchPropellor a)) h ()
