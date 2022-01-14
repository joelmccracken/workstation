{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Propellor.Property (
	-- * Property combinators
	  requires
	, before
	, onChange
	, onChangeFlagOnFail
	, flagFile
	, flagFile'
	, check
	, fallback
	, revert
	-- * Property descriptions
	, describe
	, (==>)
	-- * Constructing properties
	, Propellor
	, property
	, property'
	, OuterMetaTypesWitness
	, ensureProperty
	, pickOS
	, withOS
	, unsupportedOS
	, unsupportedOS'
	, makeChange
	, noChange
	, doNothing
	, impossible
	, endAction
	-- * Property result checking
	, UncheckedProperty
	, unchecked
	, changesFile
	, changesFileContent
	, isNewerThan
	, checkResult
	, Checkable
	, assume
) where

import System.FilePath
import Control.Monad
import Data.Monoid
import Control.Monad.IfElse
import "mtl" Control.Monad.RWS.Strict
import System.Posix.Files
import Data.Maybe
import Data.List
import Data.Hashable
import Control.Applicative
import GHC.Stack
import Prelude

import Propellor.Types
import Propellor.Types.Core
import Propellor.Types.ResultCheck
import Propellor.Types.MetaTypes
import Propellor.Types.Singletons
import Propellor.Info
import Propellor.Message
import Propellor.EnsureProperty
import Utility.Exception
import Utility.Monad
import Utility.Directory
import Utility.Misc

-- | Makes a perhaps non-idempotent Property be idempotent by using a flag
-- file to indicate whether it has run before.
-- Use with caution.
flagFile :: Property i -> FilePath -> Property i
flagFile p = flagFile' p . return

flagFile' :: Property i -> IO FilePath -> Property i
flagFile' p getflagfile = adjustPropertySatisfy p $ \satisfy -> do
	flagfile <- liftIO getflagfile
	go satisfy flagfile =<< liftIO (doesFileExist flagfile)
  where
	go _ _ True = return NoChange
	go satisfy flagfile False = do
		r <- satisfy
		when (r == MadeChange) $ liftIO $
			unlessM (doesFileExist flagfile) $ do
				createDirectoryIfMissing True (takeDirectory flagfile)
				writeFile flagfile ""
		return r

-- | Indicates that the first property depends on the second,
-- so before the first is ensured, the second must be ensured.
--
-- The combined property uses the description of the first property.
requires :: Combines x y => x -> y -> CombinedType x y
requires = combineWith
	-- Run action of y, then x
	(flip (<>))
	-- When reverting, run in reverse order.
	(<>)

-- | Combines together two properties, resulting in one property
-- that ensures the first, and if the first succeeds, ensures the second.
--
-- The combined property uses the description of the first property.
before :: Combines x y => x -> y -> CombinedType x y
before = combineWith
	-- Run action of x, then y
	(<>)
	-- When reverting, run in reverse order.
	(flip (<>))

-- | Whenever a change has to be made for a Property, causes a hook
-- Property to also be run, but not otherwise.
onChange
	:: (Combines x y)
	=> x
        -> y
        -> CombinedType x y
onChange = combineWith combiner revertcombiner
  where
	combiner (Just p) (Just hook) = Just $ do
		r <- p
		case r of
			MadeChange -> do
				r' <- hook
				return $ r <> r'
			_ -> return r
	combiner (Just p) Nothing = Just p
	combiner Nothing _ = Nothing
	revertcombiner = (<>)

-- | Same as `onChange` except that if property y fails, a flag file
-- is generated. On next run, if the flag file is present, property y
-- is executed even if property x doesn't change.
--
-- With `onChange`, if y fails, the property x `onChange` y returns
-- `FailedChange`. But if this property is applied again, it returns
-- `NoChange`. This behavior can cause trouble...
onChangeFlagOnFail
	:: (Combines x y)
	=> FilePath
        -> x
        -> y
        -> CombinedType x y
onChangeFlagOnFail flagfile = combineWith combiner revertcombiner
  where
	combiner (Just s1) s2 = Just $ do
		r1 <- s1
		case r1 of
			MadeChange -> flagFailed s2
			_ -> ifM (liftIO $ doesFileExist flagfile)
				( flagFailed s2
				, return r1
				)
	combiner Nothing _ = Nothing

	revertcombiner = (<>)

	flagFailed (Just s) = do
		r <- s
		liftIO $ case r of
			FailedChange -> createFlagFile
			_ -> removeFlagFile
		return r
	flagFailed Nothing = return NoChange

	createFlagFile = unlessM (doesFileExist flagfile) $ do
		createDirectoryIfMissing True (takeDirectory flagfile)
		writeFile flagfile ""
	
	removeFlagFile = whenM (doesFileExist flagfile) $ removeFile flagfile

-- | Changes the description of a property.
describe :: IsProp p => p -> Desc -> p
describe = setDesc

-- | Alias for @flip describe@
(==>) :: IsProp (Property i) => Desc -> Property i -> Property i
(==>) = flip describe
infixl 1 ==>

-- | Tries the first property, but if it fails to work, instead uses
-- the second.
fallback :: (Combines p1 p2) => p1 -> p2 -> CombinedType p1 p2
fallback = combineWith combiner revertcombiner
  where
	combiner (Just a1) (Just a2) = Just $ do
		r <- a1
		if r == FailedChange
			then a2
			else return r
	combiner (Just a1) Nothing = Just a1
	combiner Nothing _ = Nothing
	revertcombiner = (<>)

-- | Indicates that a Property may change a particular file. When the file
-- is modified in any way (including changing its permissions or mtime),
-- the property will return MadeChange instead of NoChange.
changesFile :: Checkable p i => p i -> FilePath -> Property i
changesFile p f = checkResult getstat comparestat p
  where
	getstat = catchMaybeIO $ getSymbolicLinkStatus f
	comparestat oldstat = do
		newstat <- getstat
		return $ if samestat oldstat newstat then NoChange else MadeChange
	samestat Nothing Nothing = True
	samestat (Just a) (Just b) = and
		-- everything except for atime
		[ deviceID a == deviceID b
		, fileID a == fileID b
		, fileMode a == fileMode b
		, fileOwner a == fileOwner b
		, fileGroup a == fileGroup b
		, specialDeviceID a == specialDeviceID b
		, fileSize a == fileSize b
		, modificationTimeHiRes a == modificationTimeHiRes b
		, isBlockDevice a == isBlockDevice b
		, isCharacterDevice a == isCharacterDevice b
		, isNamedPipe a == isNamedPipe b
		, isRegularFile a == isRegularFile b
		, isDirectory a == isDirectory b
		, isSymbolicLink a == isSymbolicLink b
		, isSocket a == isSocket b
		]
	samestat _ _ = False

-- | Like `changesFile`, but compares the content of the file.
-- Changes to mtime etc that do not change file content are treated as
-- NoChange.
changesFileContent :: Checkable p i => p i -> FilePath -> Property i
changesFileContent p f = checkResult gethash comparehash p
  where
	gethash = catchMaybeIO $ hash <$> readFileStrict f
	comparehash oldhash = do
		newhash <- gethash
		return $ if oldhash == newhash then NoChange else MadeChange

-- | Determines if the first file is newer than the second file.
--
-- This can be used with `check` to only run a command when a file
-- has changed.
--
-- > check ("/etc/aliases" `isNewerThan` "/etc/aliases.db")
-- > 	(cmdProperty "newaliases" [] `assume` MadeChange) -- updates aliases.db
--
-- Or it can be used with `checkResult` to test if a command made a change.
--
-- > checkResult (return ())
-- > 	(\_ -> "/etc/aliases.db" `isNewerThan` "/etc/aliases")
-- > 	(cmdProperty "newaliases" [])
--
-- (If one of the files does not exist, the file that does exist is
-- considered to be the newer of the two.)
isNewerThan :: FilePath -> FilePath -> IO Bool
isNewerThan x y = do
	mx <- mtime x
	my <- mtime y
	return (mx > my)
  where
	mtime f = catchMaybeIO $ modificationTimeHiRes <$> getFileStatus f

-- | Picks one of the two input properties to use,
-- depending on the targeted OS.
--
-- If both input properties support the targeted OS, then the
-- first will be used.
--
-- The resulting property will use the description of the first property
-- no matter which property is used in the end. So, it's often a good
-- idea to change the description to something clearer.
--
-- For example:
--
-- > upgraded :: Property (DebianLike + FreeBSD)
-- > upgraded = (Apt.upgraded `pickOS` Pkg.upgraded)
-- > 	`describe` "OS upgraded"
--
-- If neither input property supports the targeted OS, calls
-- `unsupportedOS`. Using the example above on a Fedora system would
-- fail that way.
pickOS
	::
		HasCallStack =>
		( SingKind ('KProxy :: KProxy ka)
		, SingKind ('KProxy :: KProxy kb)
		, DemoteRep ('KProxy :: KProxy ka) ~ [MetaType]
		, DemoteRep ('KProxy :: KProxy kb) ~ [MetaType]
		, SingI c
		-- Would be nice to have this constraint, but
		-- union will not generate metatypes lists with the same
		-- order of OS's as is used everywhere else. So,
		-- would need a type-level sort.
		--, Union a b ~ c
		)
	=> Property (MetaTypes (a :: ka))
	-> Property (MetaTypes (b :: kb))
	-> Property (MetaTypes c)
pickOS a b = c `addChildren` [toChildProperty a, toChildProperty b]
  where
	-- This use of getSatisfy is safe, because both a and b
	-- are added as children, so their info will propigate.
	c = property (getDesc a) $ do
		o <- getOS
		if matching o a
			then maybe (pure NoChange) id (getSatisfy a)
			else if matching o b
				then maybe (pure NoChange) id (getSatisfy b)
				else unsupportedOS'
	matching Nothing _ = False
	matching (Just o) p =
		Targeting (systemToTargetOS o)
			`elem`
		fromSing (proptype p)
	proptype (Property t _ _ _ _) = t

-- | Makes a property that is satisfied differently depending on specifics
-- of the host's operating system.
--
-- > myproperty :: Property Debian
-- > myproperty = withOS "foo installed" $ \w o -> case o of
-- > 	(Just (System (Debian kernel (Stable release)) arch)) -> ensureProperty w ...
-- > 	(Just (System (Debian kernel suite) arch)) -> ensureProperty w ...
-- >	_ -> unsupportedOS'
--
-- Note that the operating system specifics may not be declared for all hosts,
-- which is where Nothing comes in.
withOS
	:: (SingI metatypes)
	=> Desc
	-> (OuterMetaTypesWitness metatypes -> Maybe System -> Propellor Result)
	-> Property (MetaTypes metatypes)
withOS desc a = property' desc $ \w -> a w =<< getOS

-- | A property that always fails with an unsupported OS error.
unsupportedOS :: Property UnixLike
unsupportedOS = property "unsupportedOS" unsupportedOS'

-- | Throws an error, for use in `withOS` when a property is lacking
-- support for an OS.
unsupportedOS' :: HasCallStack => Propellor Result
unsupportedOS' = go =<< getOS
	  where
		go Nothing = error "Unknown host OS is not supported by this property."
		go (Just o) = error $ "This property is not implemented for " ++ show o

-- | Undoes the effect of a RevertableProperty.
revert :: RevertableProperty setup undo -> RevertableProperty undo setup
revert (RevertableProperty p1 p2) = RevertableProperty p2 p1

makeChange :: IO () -> Propellor Result
makeChange a = liftIO a >> return MadeChange

noChange :: Propellor Result
noChange = return NoChange

-- | A no-op property.
--
-- This is the same as `mempty` from the `Monoid` instance.
doNothing :: SingI t => Property (MetaTypes t)
doNothing = mempty

-- | In situations where it's not possible to provide a property that
-- works, this can be used to make a property that always fails with an
-- error message you provide.
impossible :: SingI t => String -> Property (MetaTypes t)
impossible msg = property "impossible" $ errorMessage msg

-- | Registers an action that should be run at the very end, after
-- propellor has checks all the properties of a host.
endAction :: Desc -> (Result -> Propellor Result) -> Propellor ()
endAction desc a = tell [EndAction desc a]
