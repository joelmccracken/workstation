-- | Maintainer: 2016 Evan Cofsky <evan@theunixman.com>
-- 
-- FreeBSD pkgng properties

{-# Language ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Propellor.Property.FreeBSD.Pkg where

import Propellor.Base
import Propellor.Types.Info

import qualified Data.Semigroup as Sem

noninteractiveEnv :: [([Char], [Char])]
noninteractiveEnv = [("ASSUME_ALWAYS_YES", "yes")]

pkgCommand :: String -> [String] -> (String, [String])
pkgCommand cmd args = ("pkg", (cmd:args))

runPkg :: String -> [String] -> IO [String]
runPkg cmd args =
	let
		(p, a) = pkgCommand cmd args
	in
		lines <$> readProcess p a

pkgCmdProperty :: String -> [String] -> UncheckedProperty FreeBSD
pkgCmdProperty cmd args = tightenTargets $ 
	let
		(p, a) = pkgCommand cmd args
	in
		cmdPropertyEnv p a noninteractiveEnv

pkgCmd :: String -> [String] -> IO [String]
pkgCmd cmd args =
	let
		(p, a) = pkgCommand cmd args
	in
		lines <$> readProcessEnv p a (Just noninteractiveEnv)

newtype PkgUpdate = PkgUpdate String
	deriving (Typeable, Sem.Semigroup, Monoid, Show)
instance IsInfo PkgUpdate where
	propagateInfo _ = PropagateInfo False

pkgUpdated :: PkgUpdate -> Bool
pkgUpdated (PkgUpdate _) = True

update :: Property (HasInfo + FreeBSD)
update =
	let
		upd = pkgCmd "update" []
		go = ifM (pkgUpdated <$> askInfo) ((noChange), (liftIO upd >> return MadeChange))
	in
		(property "pkg update has run" go :: Property FreeBSD)
			`setInfoProperty` (toInfo (PkgUpdate ""))

newtype PkgUpgrade = PkgUpgrade String
	deriving (Typeable, Sem.Semigroup, Monoid, Show)

instance IsInfo PkgUpgrade where
	propagateInfo _ = PropagateInfo False

pkgUpgraded :: PkgUpgrade -> Bool
pkgUpgraded (PkgUpgrade _) = True

upgrade :: Property (HasInfo + FreeBSD)
upgrade =
	let
		upd = pkgCmd "upgrade" []
		go = ifM (pkgUpgraded <$> askInfo) ((noChange), (liftIO upd >> return MadeChange))
	in
		(property "pkg upgrade has run" go :: Property FreeBSD)
			`setInfoProperty` (toInfo (PkgUpdate ""))
			`requires` update

type Package = String

installed :: Package -> Property FreeBSD
installed pkg = check (isInstallable pkg) $ pkgCmdProperty "install" [pkg]

isInstallable :: Package -> IO Bool
isInstallable p = (not <$> isInstalled p) <&&> exists p

isInstalled :: Package -> IO Bool
isInstalled p = (runPkg "info" [p] >> return True)
	`catchIO` (\_ -> return False)

exists :: Package -> IO Bool
exists p = (runPkg "search" ["--search", "name", "--exact", p] >> return True)
	`catchIO` (\_ -> return False)
