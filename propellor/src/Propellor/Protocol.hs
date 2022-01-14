-- | This is a simple line-based protocol used for communication between
-- a local and remote propellor. It's sent over a ssh channel, and lines of
-- the protocol can be interspersed with other, non-protocol lines
-- that should be passed through to be displayed.
--
-- Avoid making backwards-incompatible changes to this protocol,
-- since propellor needs to use this protocol to update itself to new
-- versions speaking newer versions of the protocol.

module Propellor.Protocol where

import Data.List

import Propellor.Base

data Stage = NeedGitClone | NeedRepoUrl | NeedPrivData | NeedGitPush | NeedPrecompiled
	deriving (Read, Show, Eq)

type Marker = String
type Marked = String

statusMarker :: Marker
statusMarker = "STATUS"

privDataMarker :: String
privDataMarker = "PRIVDATA "

repoUrlMarker :: String
repoUrlMarker = "REPOURL "

gitPushMarker :: String
gitPushMarker = "GITPUSH"

toMarked :: Marker -> String -> String
toMarked = (++)

fromMarked :: Marker -> Marked -> Maybe String
fromMarked marker s
	| marker `isPrefixOf` s = Just $ drop (length marker) s
	| otherwise = Nothing

sendMarked :: Handle -> Marker -> String -> IO ()
sendMarked h marker s = do
	debug ["sent marked", marker]
	sendMarked' h marker s

sendMarked' :: Handle -> Marker -> String -> IO ()
sendMarked' h marker s = do
	-- Prefix string with newline because sometimes a
	-- incomplete line has been output, and the marker needs to
	-- come at the start of a line.
	hPutStrLn h ("\n" ++ toMarked marker s)
	hFlush h

getMarked :: Handle -> Marker -> IO (Maybe String)
getMarked h marker = go =<< catchMaybeIO (hGetLine h)
  where
	go Nothing = return Nothing
	go (Just l) = case fromMarked marker l of
		Nothing -> do
			unless (null l) $
				hPutStrLn stderr l
			getMarked h marker
		Just v -> do
			debug ["received marked", marker]
			return (Just v)

req :: Stage -> Marker -> (String -> IO ()) -> IO ()
req stage marker a = do
	debug ["requested marked", marker]
	sendMarked' stdout statusMarker (show stage)
	maybe noop a =<< getMarked stdin marker
