{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Propellor.Property.Atomic (
	atomicDirUpdate,
	atomicDirSync,
	atomicUpdate,
	AtomicResourcePair(..),
	flipAtomicResourcePair,
	SwapAtomicResourcePair,
	CheckAtomicResourcePair,
) where

import Propellor.Base
import Propellor.Types.Core
import Propellor.Types.MetaTypes
import Propellor.EnsureProperty
import Propellor.Property.File
import Propellor.Property.Rsync (syncDir)

import System.Posix.Files

-- | A pair of resources, one active and one inactive, which can swap
-- positions atomically.
data AtomicResourcePair a = AtomicResourcePair
	{ activeAtomicResource :: a
	, inactiveAtomicResource :: a
	}

flipAtomicResourcePair :: AtomicResourcePair a -> AtomicResourcePair a
flipAtomicResourcePair a = AtomicResourcePair
	{ activeAtomicResource = inactiveAtomicResource a
	, inactiveAtomicResource = activeAtomicResource a
	}

-- | Action that activates the inactiveAtomicResource, and deactivates
-- the activeAtomicResource. This action must be fully atomic.
type SwapAtomicResourcePair a = AtomicResourcePair a -> Propellor Bool

-- | Checks which of the pair of resources is currently active and
-- which is inactive, and puts them in the correct poisition in
-- the AtomicResourcePair.
type CheckAtomicResourcePair a = AtomicResourcePair a -> Propellor (AtomicResourcePair a)

-- | Makes a non-atomic Property be atomic, by applying it to the 
-- inactiveAtomicResource, and if it was successful,
-- atomically activating that resource.
atomicUpdate
	-- Constriaint inherited from ensureProperty.
	:: EnsurePropertyAllowed t t
	=> SingI t
	=> AtomicResourcePair a
	-> CheckAtomicResourcePair a
	-> SwapAtomicResourcePair a
	-> (a -> Property (MetaTypes t))
	-> Property (MetaTypes t)
atomicUpdate rbase rcheck rswap mkp = property' d $ \w -> do
	r <- rcheck rbase
	res <- ensureProperty w $ mkp $ inactiveAtomicResource r
	case res of
		FailedChange -> return FailedChange
		NoChange -> return NoChange
		MadeChange -> do
			ok <- rswap r
			if ok
				then return res
				else return FailedChange
  where
	d = getDesc $ mkp $ activeAtomicResource rbase

-- | Applies a Property to a directory such that the directory is updated
-- fully atomically; there is no point in time in which the directory will
-- be in an inconsistent state.
--
-- For example, git repositories are not usually updated atomically,
-- and so while the repository is being updated, the files in it can be a
-- mixture of two different versions, which could cause unexpected
-- behavior to consumers. To avoid such problems:
--
-- >	& atomicDirUpdate "/srv/web/example.com"
-- >		(\d -> Git.pulled "joey" "http://.." d Nothing)
--
-- This operates by making a second copy of the directory, and passing it
-- to the Property, which can make whatever changes it needs to that copy,
-- non-atomically. After the Property successfully makes a change, the
-- copy is swapped into place, fully atomically.
--
-- This necessarily uses double the disk space, since there are two copies
-- of the directory. The parent directory will actually contain three
-- children: a symlink with the name of the directory itself, and two copies
-- of the directory, with names suffixed with ".1" and ".2"
atomicDirUpdate
	-- Constriaint inherited from ensureProperty.
	:: EnsurePropertyAllowed t t
	=> SingI t
	=> FilePath
	-> (FilePath -> Property (MetaTypes t))
	-> Property (MetaTypes t)
atomicDirUpdate d = atomicUpdate (mkDirLink d) (checkDirLink d) (swapDirLink d)

mkDirLink :: FilePath -> AtomicResourcePair FilePath
mkDirLink d = AtomicResourcePair
	{ activeAtomicResource = addext ".1"
	, inactiveAtomicResource = addext ".2"
	}
  where
	addext = addExtension (dropTrailingPathSeparator d)

inactiveLinkTarget :: AtomicResourcePair FilePath -> FilePath
inactiveLinkTarget = takeFileName . inactiveAtomicResource

swapDirLink :: FilePath -> SwapAtomicResourcePair FilePath
swapDirLink d rp = liftIO $ do
	v <- tryIO $ createSymbolicLink (inactiveLinkTarget rp)
		`viaStableTmp` d
	case v of
		Right () -> return True
		Left e -> do
			warningMessage $ "Unable to update symlink at " ++ d ++ " (" ++ show e ++ ")"
			return False

checkDirLink :: FilePath -> CheckAtomicResourcePair FilePath
checkDirLink d rp = liftIO $ do
	v <- tryIO $ readSymbolicLink d
	return $ case v of
		Right t | t == inactiveLinkTarget rp ->
			flipAtomicResourcePair rp
		_ -> rp

-- | This can optionally be used after atomicDirUpdate to rsync the changes
-- that were made over to the other copy of the directory. It's not
-- necessary to use this, but it can improve efficiency.
--
-- For example:
--
-- >	& atomicDirUpdate "/srv/web/example.com"
-- >		(\d -> Git.pulled "joey" "http://.." d Nothing)
-- >		`onChange` atomicDirSync "/srv/web/example.com"
--
-- Using atomicDirSync in the above example lets git only download
-- the changes once, rather than the same changes being downloaded a second
-- time to update the other copy of the directory the next time propellor
-- runs.
--
-- Suppose that a web server program is run from the git repository,
-- and needs to be restarted after the pull. That restart should be done
-- after the atomicDirUpdate, but before the atomicDirSync. That way,
-- the old web server process will not have its files changed out from
-- under it.
--
-- >	& atomicDirUpdate "/srv/web/example.com"
-- >		(\d -> Git.pulled "joey" "http://.." d Nothing)
-- >		`onChange` (webServerRestart `before` atomicDirSync "/srv/web/example.com")
atomicDirSync :: FilePath -> Property (DebianLike + ArchLinux)
atomicDirSync d = syncDir (activeAtomicResource rp) (inactiveAtomicResource rp)
  where
	rp = mkDirLink d
