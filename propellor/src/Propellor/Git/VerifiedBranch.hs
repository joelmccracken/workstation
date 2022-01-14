module Propellor.Git.VerifiedBranch where

import Propellor.Base
import Propellor.Git
import Propellor.PrivData.Paths

{- To verify origin branch commit's signature, have to convince gpg
 - to use our keyring while running git verify-tag.
 - Which has no way to pass options to gpg. Argh!
 -}
verifyOriginBranch :: String -> IO Bool
verifyOriginBranch originbranch = do
	let gpgconf = privDataDir </> "gpg.conf"
	keyring <- privDataKeyring
	writeFile gpgconf $ unlines
		[ " keyring " ++ keyring
		, "no-auto-check-trustdb"
		]
	-- gpg is picky about perms
	modifyFileMode privDataDir (removeModes otherGroupModes)
	verified <- boolSystemEnv "git" [Param "verify-commit", Param originbranch]
		(Just [("GNUPGHOME", privDataDir)])
	nukeFile $ privDataDir </> "trustdb.gpg"
	nukeFile $ privDataDir </> "pubring.gpg"
	nukeFile $ privDataDir </> "gpg.conf"
	return verified

-- Returns True if HEAD is changed by fetching and merging from origin.
fetchOrigin :: IO Bool
fetchOrigin = do
	fetched <- actionMessage "Pull from central git repository" $
		boolSystem "git" [Param "fetch"]
	if fetched
		then mergeOrigin
		else return False

mergeOrigin :: IO Bool
mergeOrigin = do
	branchref <- getCurrentBranch
	let originbranch = "origin" </> branchref

	oldsha <- getCurrentGitSha1 branchref

	keyring <- privDataKeyring
	whenM (doesFileExist keyring) $
		ifM (verifyOriginBranch originbranch)
			( do
				putStrLn $ "git branch " ++ originbranch ++ " gpg signature verified; merging"
				hFlush stdout
				void $ boolSystem "git" [Param "merge", Param originbranch]
			, warningMessage $ "git branch " ++ originbranch ++ " is not signed with a trusted gpg key; refusing to deploy it! (Running with previous configuration instead.)"
			)

	newsha <- getCurrentGitSha1 branchref
	return $ oldsha /= newsha
