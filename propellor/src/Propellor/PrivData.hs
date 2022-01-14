{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Propellor.PrivData (
	withPrivData,
	withSomePrivData,
	addPrivData,
	setPrivData,
	unsetPrivData,
	unsetPrivDataUnused,
	dumpPrivData,
	editPrivData,
	filterPrivData,
	listPrivDataFields,
	makePrivDataDir,
	decryptPrivData,
	readPrivData,
	readPrivDataFile,
	PrivMap,
	PrivInfo,
	forceHostContext,
) where

import System.IO
import Data.Maybe
import Data.List
import Data.Typeable
import Control.Monad
import Control.Monad.IfElse
import "mtl" Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Data.Monoid
import Data.Semigroup as Sem
import Prelude

import Propellor.Types
import Propellor.Types.PrivData
import Propellor.Types.MetaTypes
import Propellor.Types.Info
import Propellor.Message
import Propellor.Info
import Propellor.Gpg
import Propellor.PrivData.Paths
import Utility.Monad
import Utility.PartialPrelude
import Utility.Exception
import Utility.Tmp
import Utility.SafeCommand
import Utility.Process.NonConcurrent
import Utility.Misc
import Utility.FileMode
import Utility.Env
import Utility.Table
import Utility.Directory

-- | Allows a Property to access the value of a specific PrivDataField,
-- for use in a specific Context or HostContext.
--
-- Example use:
--
-- > withPrivData (PrivFile pemfile) (Context "joeyh.name") $ \getdata ->
-- >     property "joeyh.name ssl cert" $ getdata $ \privdata ->
-- >       liftIO $ writeFile pemfile (privDataVal privdata)
-- >   where pemfile = "/etc/ssl/certs/web.pem"
-- 
-- Note that if the value is not available, the action is not run
-- and instead it prints a message to help the user make the necessary
-- private data available.
--
-- The resulting Property includes Info about the PrivDataField
-- being used, which is necessary to ensure that the privdata is sent to
-- the remote host by propellor.
withPrivData
	::
		( IsContext c
		, IsPrivDataSource s
		, IncludesInfo metatypes ~ 'True
		)
	=> s
	-> c
	-> (((PrivData -> Propellor Result) -> Propellor Result) -> Property metatypes)
	-> Property metatypes
withPrivData s = withPrivData' snd [s]

-- Like withPrivData, but here any one of a list of PrivDataFields can be used.
withSomePrivData
	::
		( IsContext c
		, IsPrivDataSource s
		, IncludesInfo metatypes ~ 'True
		)
	=> [s]
	-> c
	-> ((((PrivDataField, PrivData) -> Propellor Result) -> Propellor Result) -> Property metatypes)
	-> Property metatypes
withSomePrivData = withPrivData' id

withPrivData' 
	::
		( IsContext c
		, IsPrivDataSource s
		, IncludesInfo metatypes ~ 'True
		)
	=> ((PrivDataField, PrivData) -> v)
	-> [s]
	-> c
	-> (((v -> Propellor Result) -> Propellor Result) -> Property metatypes)
	-> Property metatypes
withPrivData' feed srclist c mkprop = addinfo $ mkprop $ \a ->
	maybe missing (a . feed) =<< getM get fieldlist
  where
  	get field = do
		context <- mkHostContext hc <$> asks hostName
		maybe Nothing (\privdata -> Just (field, privdata))
			<$> liftIO (getLocalPrivData field context)
	missing = do
		Context cname <- mkHostContext hc <$> asks hostName
		warningMessage $ "Missing privdata " ++ intercalate " or " fieldnames ++ " (for " ++ cname ++ ")"
		infoMessage $ 
			"Fix this by running:" :
			showSet (map (\s -> (privDataField s, Context cname, describePrivDataSource s)) srclist)
		return FailedChange
	addinfo p = p `addInfoProperty` (toInfo privset)
	privset = PrivInfo $ S.fromList $
		map (\s -> (privDataField s, describePrivDataSource s, hc)) srclist
	fieldnames = map show fieldlist
	fieldlist = map privDataField srclist
	hc = asHostContext c

showSet :: [(PrivDataField, Context, Maybe PrivDataSourceDesc)] -> [String]
showSet = concatMap go
  where
	go (f, Context c, md) = catMaybes
		[ Just $ "  propellor --set '" ++ show f ++ "' '" ++ c ++ "' \\"
		, maybe Nothing (\d -> Just $ "    " ++ d) md
		, Just ""
		]

addPrivData :: (PrivDataField, Maybe PrivDataSourceDesc, HostContext) -> Property (HasInfo + UnixLike)
addPrivData v = pureInfoProperty (show v) (PrivInfo (S.singleton v))

{- Gets the requested field's value, in the specified context if it's
 - available, from the host's local privdata cache. -}
getLocalPrivData :: PrivDataField -> Context -> IO (Maybe PrivData)
getLocalPrivData field context =
	getPrivData field context . fromMaybe M.empty <$> localcache
  where
	localcache = catchDefaultIO Nothing $ readish <$> readFile privDataLocal

type PrivMap = M.Map (PrivDataField, Context) String

-- | Get only the set of PrivData that the Host's Info says it uses.
filterPrivData :: Host -> PrivMap -> PrivMap
filterPrivData host = M.filterWithKey (\k _v -> S.member k used)
  where
	used = S.map (\(f, _, c) -> (f, mkHostContext c (hostName host))) $
		fromPrivInfo $ fromInfo $ hostInfo host

getPrivData :: PrivDataField -> Context -> PrivMap -> Maybe PrivData
getPrivData field context m = do
	s <- M.lookup (field, context) m
	return (PrivData s)

setPrivData :: PrivDataField -> Context -> IO ()
setPrivData field context = do
	putStrLn "Enter private data on stdin; ctrl-D when done:"
	setPrivDataTo field context . PrivData =<< hGetContentsStrict stdin

unsetPrivData :: PrivDataField -> Context -> IO ()
unsetPrivData field context = do
	modifyPrivData $ M.delete (field, context)
	descUnset field context

descUnset :: PrivDataField -> Context -> IO ()
descUnset field context =
	putStrLn $ "Private data unset: " ++ show field ++ " " ++ show context

unsetPrivDataUnused :: [Host] -> IO ()
unsetPrivDataUnused hosts = do
	deleted <- modifyPrivData' $ \m ->
		let (keep, del) = M.partitionWithKey (\k _ -> k `M.member` usedby) m
		in (keep, M.keys del)
	mapM_ (uncurry descUnset) deleted
  where
	usedby = mkUsedByMap hosts

dumpPrivData :: PrivDataField -> Context -> IO ()
dumpPrivData field context = do
	maybe (error "Requested privdata is not set.")
		(L.hPut stdout . privDataByteString)
		=<< (getPrivData field context <$> decryptPrivData)

editPrivData :: PrivDataField -> Context -> IO ()
editPrivData field context = do
	v <- getPrivData field context <$> decryptPrivData
	v' <- withTmpFile "propellorXXXX" $ \f th -> do
		hClose th
		maybe noop (\p -> writeFileProtected' f (`L.hPut` privDataByteString p)) v
		editor <- getEnvDefault "EDITOR" "vi"
		unlessM (boolSystemNonConcurrent editor [File f]) $
			error "Editor failed; aborting."
		PrivData <$> readFile f
	setPrivDataTo field context v'

listPrivDataFields :: [Host] -> IO ()
listPrivDataFields hosts = do
	m <- decryptPrivData
	
	section "Currently set data:"
	showtable $ map mkrow (M.keys m)
	let missing = M.keys $ M.difference wantedmap m
	
	unless (null missing) $ do
		section "Missing data that would be used if set:"
		showtable $ map mkrow missing

		section "How to set missing data:"
		mapM_ putStrLn $ showSet $
			map (\(f, c) -> (f, c, join $ M.lookup (f, c) descmap)) missing
  where
	header = ["Field", "Context", "Used by"]
	mkrow k@(field, Context context) =
		[ shellEscape $ show field
		, shellEscape context
		, intercalate ", " $ sort $ fromMaybe [] $ M.lookup k usedby
		]
	usedby = mkUsedByMap hosts
	wantedmap = M.fromList $ zip (M.keys usedby) (repeat "")
	descmap = M.unions $ map (`mkPrivDataMap` id) hosts
	section desc = putStrLn $ "\n" ++ desc
	showtable rows = do
		putStr $ unlines $ formatTable $ tableWithHeader header rows

mkUsedByMap :: [Host] -> M.Map (PrivDataField, Context) [HostName]
mkUsedByMap = M.unionsWith (++) . map (\h -> mkPrivDataMap h $ const [hostName h])

mkPrivDataMap :: Host -> (Maybe PrivDataSourceDesc -> a) -> M.Map (PrivDataField, Context) a
mkPrivDataMap host mkv = M.fromList $
	map (\(f, d, c) -> ((f, mkHostContext c (hostName host)), mkv d))
		(S.toList $ fromPrivInfo $ fromInfo $ hostInfo host)

setPrivDataTo :: PrivDataField -> Context -> PrivData -> IO ()
setPrivDataTo field context (PrivData value) = do
	modifyPrivData set
	putStrLn "Private data set."
  where
	set = M.insert (field, context) value

modifyPrivData :: (PrivMap -> PrivMap) -> IO ()
modifyPrivData f = modifyPrivData' (\m -> (f m, ()))

modifyPrivData' :: (PrivMap -> (PrivMap, a)) -> IO a
modifyPrivData' f = do
	makePrivDataDir
	m <- decryptPrivData
	let (m', r) = f m
	privdata <- privDataFile
	gpgEncrypt privdata (show m')
	void $ boolSystem "git" [Param "add", File privdata]
	return r

decryptPrivData :: IO PrivMap
decryptPrivData = readPrivData <$> (gpgDecrypt =<< privDataFile)

readPrivData :: String -> PrivMap
readPrivData = fromMaybe M.empty . readish

readPrivDataFile :: FilePath -> IO PrivMap
readPrivDataFile f = readPrivData <$> readFileStrict f

makePrivDataDir :: IO ()
makePrivDataDir = createDirectoryIfMissing False privDataDir

newtype PrivInfo = PrivInfo
	{ fromPrivInfo :: S.Set (PrivDataField, Maybe PrivDataSourceDesc, HostContext) }
	deriving (Eq, Ord, Show, Typeable, Sem.Semigroup, Monoid)

-- PrivInfo always propagates out of containers, so that propellor
-- can see which hosts need it.
instance IsInfo PrivInfo where
	propagateInfo _ = PropagatePrivData

-- | Sets the context of any privdata that uses HostContext to the
-- provided name.
forceHostContext :: String -> PrivInfo -> PrivInfo
forceHostContext name i = PrivInfo $ S.map go (fromPrivInfo i)
  where
	go (f, d, HostContext ctx) = (f, d, HostContext (const $ ctx name)) 
