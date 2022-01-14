-- | This module handles all display of output to the console when
-- propellor is ensuring Properties.
--
-- When two threads both try to display a message concurrently, 
-- the messages will be displayed sequentially.

module Propellor.Message (
	Trace(..),
	parseTrace,
	getMessageHandle,
	isConsole,
	forceConsole,
	actionMessage,
	actionMessageOn,
	warningMessage,
	infoMessage,
	errorMessage,
	stopPropellorMessage,
	messagesDone,
	createProcessConcurrent,
	withConcurrentOutput,
) where

import System.Console.ANSI
import System.IO
import Control.Monad.IfElse
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import System.Console.Concurrent
import Control.Applicative
import Prelude

import Propellor.Types
import Propellor.Types.Exception
import Utility.Monad
import Utility.Env
import Utility.Exception
import Utility.PartialPrelude

-- | Serializable tracing. Export `PROPELLOR_TRACE=1` in the environment to
-- make propellor emit these to stdout, in addition to its other output.
data Trace 
	= ActionStart (Maybe HostName) Desc
	| ActionEnd (Maybe HostName) Desc Result
	deriving (Read, Show)

-- | Given a line read from propellor, if it's a serialized Trace,
-- parses it.
parseTrace :: String -> Maybe Trace
parseTrace = readish

data MessageHandle = MessageHandle
	{ isConsole :: Bool
	, traceEnabled :: Bool
	}

-- | A shared global variable for the MessageHandle.
{-# NOINLINE globalMessageHandle #-}
globalMessageHandle :: MVar MessageHandle
globalMessageHandle = unsafePerformIO $ 
	newMVar =<< MessageHandle
		<$> catchDefaultIO False (hIsTerminalDevice stdout)
		<*> ((== Just "1") <$> getEnv "PROPELLOR_TRACE")

-- | Gets the global MessageHandle.
getMessageHandle :: IO MessageHandle
getMessageHandle = readMVar globalMessageHandle

trace :: Trace -> IO ()
trace t = whenM (traceEnabled <$> getMessageHandle) $
	putStrLn $ show t

-- | Force console output. This can be used when stdout is not directly
-- connected to a console, but is eventually going to be displayed at a
-- console.
forceConsole :: IO ()
forceConsole = modifyMVar_ globalMessageHandle $ \mh ->
	pure (mh { isConsole = True })

whenConsole :: String -> IO String
whenConsole s = ifM (isConsole <$> getMessageHandle)
	( pure s
	, pure ""
	)

-- | Shows a message while performing an action, with a colored status
-- display.
actionMessage :: (MonadIO m, MonadMask m, ActionResult r, ToResult r) => Desc -> m r -> m r
actionMessage = actionMessage' Nothing

-- | Shows a message while performing an action on a specified host,
-- with a colored status display.
actionMessageOn :: (MonadIO m, MonadMask m, ActionResult r, ToResult r) => HostName -> Desc -> m r -> m r
actionMessageOn = actionMessage' . Just

actionMessage' :: (MonadIO m, ActionResult r, ToResult r) => Maybe HostName -> Desc -> m r -> m r
actionMessage' mhn desc a = do
	liftIO $ trace $ ActionStart mhn desc
	liftIO $ outputConcurrent
		=<< whenConsole (setTitleCode $ "propellor: " ++ desc)

	r <- a

	liftIO $ outputConcurrent . concat =<< sequence
		[ whenConsole $
			setTitleCode "propellor: running"
		, showhn mhn
		, pure $ desc ++ " ... "
		, let (msg, intensity, color) = getActionResult r
		  in colorLine intensity color msg
		]
	liftIO $ trace $ ActionEnd mhn desc (toResult r)

	return r
  where
	showhn Nothing = return ""
	showhn (Just hn) = concat <$> sequence
		[ whenConsole $
			setSGRCode [SetColor Foreground Dull Cyan]
		, pure (hn ++ " ")
		, whenConsole $
			setSGRCode []
		]

warningMessage :: MonadIO m => String -> m ()
warningMessage s = liftIO $
	errorConcurrent =<< colorLine Vivid Magenta ("** warning: " ++ s)

infoMessage :: MonadIO m => [String] -> m ()
infoMessage ls = liftIO $ outputConcurrent $ concatMap (++ "\n") ls

-- | Displays the error message in red, and throws an exception.
--
-- When used inside a property, the exception will make the current
-- property fail. Propellor will continue to the next property.
errorMessage :: MonadIO m => String -> m a
errorMessage s = liftIO $ do
	errorConcurrent =<< colorLine Vivid Red ("** error: " ++ s)
	-- Normally this exception gets caught and is not displayed,
	-- and propellor continues. So it's only displayed if not
	-- caught, and so we say, cannot continue.
	error "Cannot continue!"
 
-- | Like `errorMessage`, but throws a `StopPropellorException`,
-- preventing propellor from continuing to the next property.
--
-- Think twice before using this. Is the problem so bad that propellor
-- cannot try to ensure other properties? If not, use `errorMessage`
-- instead.
stopPropellorMessage :: MonadIO m => String -> m a
stopPropellorMessage s = liftIO $ do
	outputConcurrent =<< colorLine Vivid Red ("** fatal error: " ++ s)
	throwM $ StopPropellorException "Cannot continue!"

colorLine :: ColorIntensity -> Color -> String -> IO String
colorLine intensity color msg = concat <$> sequence
	[ whenConsole $
		setSGRCode [SetColor Foreground intensity color]
	, pure msg
	, whenConsole $
		setSGRCode []
	-- Note this comes after the color is reset, so that
	-- the color set and reset happen in the same line.
	, pure "\n"
	]

-- | Called when all messages about properties have been printed.
messagesDone :: IO ()
messagesDone = outputConcurrent
	=<< whenConsole (setTitleCode "propellor: done")
