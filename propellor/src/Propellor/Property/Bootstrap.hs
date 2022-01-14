-- | This module contains properties that configure how Propellor
-- bootstraps to run itself on a Host.

module Propellor.Property.Bootstrap (
	Bootstrapper(..),
	Builder(..),
	bootstrapWith,
	RepoSource(..),
	bootstrappedFrom,
	clonedFrom
) where

import Propellor.Base
import Propellor.Bootstrap
import Propellor.Types.Info
import Propellor.Types.Container
import Propellor.Property.Chroot
import Propellor.PrivData.Paths

import Data.List
import qualified Data.ByteString as B

-- | This property can be used to configure the `Bootstrapper` that is used
-- to bootstrap propellor on a Host. For example, if you want to use
-- stack:
--
-- > host "example.com" $ props
-- > 	& bootstrapWith (Robustly Stack)
--
-- When `bootstrappedFrom` is used in a `Chroot` or other `Container`, 
-- this property can also be added to the chroot to configure it.
bootstrapWith :: Bootstrapper -> Property (HasInfo + UnixLike)
bootstrapWith b = pureInfoProperty desc (InfoVal b)
  where
	desc = "propellor bootstrapped with " ++ case b of
		Robustly Stack -> "stack"
		Robustly Cabal -> "cabal"
		OSOnly -> "OS packages only"

-- | Where a propellor repository should be bootstrapped from.
data RepoSource
	= GitRepoUrl String
	| GitRepoOutsideChroot
	-- ^ When used in a chroot, this copies the git repository from
	-- outside the chroot, including its configuration.

-- | Bootstraps a propellor installation into
-- /usr/local/propellor/
--
-- Normally, propellor is bootstrapped by eg, using propellor --spin,
-- and so this property is not generally needed.
--
-- This property only does anything when used inside a Chroot or other
-- Container. This is particularly useful inside a chroot used to build a
-- disk image, to make the disk image have propellor installed.
--
-- The git repository is cloned (or pulled to update if it already exists).
--
-- All build dependencies are installed, using distribution packages
-- or falling back to using cabal or stack.
bootstrappedFrom :: RepoSource -> Property Linux
bootstrappedFrom reposource = check (hasContainerCapability FilesystemContained) $
	go `requires` clonedFrom reposource
  where
	go :: Property Linux
	go = property "Propellor bootstrapped" $ do
		system <- getOS
		-- gets Host value representing the chroot this is run in
		chroothost <- ask
		-- load privdata from outside the chroot, and filter
		-- to only the privdata needed inside the chroot.
		privdata <- liftIO $ filterPrivData chroothost
			<$> readPrivDataFile privDataLocal
		bootstrapper <- getBootstrapper
		assumeChange $ exposeTrueLocaldir $ const $ do
			liftIO $ createDirectoryIfMissing True $
				takeDirectory privDataLocal
			liftIO $ writeFileProtected privDataLocal $
				show privdata
			runShellCommand $ buildShellCommand
				[ "cd " ++ localdir
				, checkDepsCommand bootstrapper system
				, buildCommand bootstrapper
				]

-- | Clones the propellor repository into /usr/local/propellor/
--
-- If the propellor repo has already been cloned, pulls to get it
-- up-to-date.
clonedFrom :: RepoSource -> Property Linux
clonedFrom reposource = case reposource of
	GitRepoOutsideChroot -> go `onChange` copygitconfig
	_ -> go
  where
	go :: Property Linux
	go = property ("Propellor repo cloned from " ++ sourcedesc) $
		ifM needclone (makeclone, updateclone)
	
	makeclone = do
		let tmpclone = localdir ++ ".tmpclone"
		system <- getOS
		assumeChange $ exposeTrueLocaldir $ \sysdir -> do
			let originloc = case reposource of
				GitRepoUrl s -> s
				GitRepoOutsideChroot -> sysdir
			runShellCommand $ buildShellCommand
				[ installGitCommand system
				, "rm -rf " ++ tmpclone
				, "git clone " ++ shellEscape originloc ++ " " ++ tmpclone
				, "mkdir -p " ++ localdir
				-- This is done rather than deleting
				-- the old localdir, because if it is bound
				-- mounted from outside the chroot, deleting
				-- it after unmounting in unshare will remove
				-- the bind mount outside the unshare.
				, "(cd " ++ tmpclone ++ " && tar c .) | (cd " ++ localdir ++ " && tar x)"
				, "rm -rf " ++ tmpclone
				]
	
	updateclone = assumeChange $ exposeTrueLocaldir $ const $
		runShellCommand $ buildShellCommand
			[ "cd " ++ localdir
			, "git pull"
			]
	
	-- Copy the git config of the repo outside the chroot into the
	-- chroot. This way it has the same remote urls, and other git
	-- configuration.
	copygitconfig :: Property Linux
	copygitconfig = property ("Propellor repo git config copied from outside the chroot") $ do
		let gitconfig = localdir </> ".git" </> "config"
		cfg <- liftIO $ B.readFile gitconfig
		exposeTrueLocaldir $ const $
			liftIO $ B.writeFile gitconfig cfg
		return MadeChange

	needclone = (hasContainerCapability FilesystemContained <&&> truelocaldirisempty)
		<||> (liftIO (not <$> doesDirectoryExist localdir))
	
	truelocaldirisempty = exposeTrueLocaldir $ const $
		runShellCommand ("test ! -d " ++ localdir ++ "/.git")

	sourcedesc = case reposource of
		GitRepoUrl s -> s
		GitRepoOutsideChroot -> localdir ++ " outside the chroot"

assumeChange :: Propellor Bool -> Propellor Result
assumeChange a = do
	ok <- a
	return (cmdResult ok <> MadeChange)

buildShellCommand :: [String] -> String
buildShellCommand = intercalate "&&" . map (\c -> "(" ++ c ++ ")")

runShellCommand :: String -> Propellor Bool
runShellCommand s = liftIO $ boolSystem "sh" [ Param "-c", Param s]
