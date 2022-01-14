{-# LANGUAGE DeriveDataTypeable #-}

-- | Properties to manipulate propellor's @/usr/local/propellor@ on spun hosts

module Propellor.Property.Localdir where

import Propellor.Base
import Propellor.Git.Config
import Propellor.Types.Info
import Propellor.Types.Container
import Propellor.Property.Mount (partialBindMountsOf, umountLazy)
import qualified Propellor.Property.Git as Git

-- | Sets the url to use as the origin of propellor's git repository.
--
-- By default, the url is taken from the deploy or origin remote of
-- the repository that propellor --spin is run in. Setting this property
-- overrides that default behavior with a different url.
--
-- When hosts are being updated without using -- --spin, eg when using
-- the `Propellor.Property.Cron.runPropellor` cron job, this property can
-- be set to redirect them to a new git repository url.
hasOriginUrl :: String -> Property (HasInfo + DebianLike)
hasOriginUrl u =
	setInfoProperty p (toInfo (InfoVal (OriginUrl u)))
		`requires` Git.installed
  where
	p :: Property UnixLike
	p = property ("propellor repo url " ++ u) $ do
		curru <- liftIO getRepoUrl
		if curru == Just u
			then return NoChange
			else makeChange $ setRepoUrl u

newtype OriginUrl = OriginUrl String
	deriving (Show, Typeable)

-- | Removes the @/usr/local/propellor@ directory used to spin the host, after
-- ensuring other properties.  Without this property, that directory is left
-- behind after the spin.
--
-- Does not perform other clean up, such as removing Haskell libraries that were
-- installed in order to build propellor, or removing cronjobs such as created
-- by 'Propellor.Property.Cron.runPropellor'.
removed :: Property UnixLike
removed = check (doesDirectoryExist localdir) $
	property "propellor's /usr/local dir to be removed" $ do
		endAction "removing /usr/local/propellor" atend
		return NoChange
  where
	atend _ = do
		ifM (hasContainerCapability FilesystemContained)
			-- In a chroot, all we have to do is unmount localdir,
			-- and then delete it
			( liftIO $ umountLazy localdir
			-- Outside of a chroot, if we don't unmount any bind
			-- mounts of localdir before deleting it, another run of
			-- propellor will have problems reestablishing those
			-- bind mounts in order to spin chroots
			, liftIO $ partialBindMountsOf localdir
				>>= mapM_ umountLazy
			)
		liftIO $ removeDirectoryRecursive localdir
		return NoChange
