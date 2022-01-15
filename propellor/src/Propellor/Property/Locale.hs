-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Locale where

import Propellor.Base
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt

import Data.List (isPrefixOf)

type Locale = String
type LocaleVariable = String

-- | Select a locale for a list of global locale variables.
--
-- A locale variable is of the form @LC_BLAH@, @LANG@ or @LANGUAGE@.  See
-- @locale(5)@.  One might say
--
--  >  & "en_GB.UTF-8" `Locale.selectedFor` ["LC_PAPER", "LC_MONETARY"]
--
-- to select the British English locale for paper size and currency conventions.
--
-- Note that reverting this property does not make a locale unavailable.  That's
-- because it might be required for other Locale.selectedFor statements.
selectedFor :: Locale -> [LocaleVariable] -> RevertableProperty DebianLike DebianLike
locale `selectedFor` vars = select <!> deselect
  where
	select = tightenTargets $ 
		check (not <$> isselected) 
			(cmdProperty "update-locale" selectArgs)
			`requires` available locale
			`describe` (locale ++ " locale selected")
	deselect = tightenTargets $
		check isselected (cmdProperty "update-locale" vars)
			`describe` (locale ++ " locale deselected")
	selectArgs = zipWith (++) vars (repeat ('=':locale))
	isselected = locale `isSelectedFor` vars

isSelectedFor :: Locale -> [LocaleVariable] -> IO Bool
locale `isSelectedFor` vars = do
	ls <- catchDefaultIO [] $ lines <$> readFile "/etc/default/locale"
	return $ and $ map (\v -> v ++ "=" ++ locale `elem` ls) vars
	

-- | Ensures a locale is generated (or, if reverted, ensure it's not).
--
-- Fails if a locale is not available to be generated.  That is, a commented out
-- entry for the locale and an accompanying charset must be present in
-- /etc/locale.gen.
--
-- Per Debian bug #684134 we cannot ensure a locale is generated by means of
-- Apt.reConfigure.  So localeAvailable edits /etc/locale.gen manually.
available :: Locale -> RevertableProperty DebianLike DebianLike
available locale = ensureAvailable `requires` Apt.installed ["locales"]
	<!> ensureUnavailable
  where
	f = "/etc/locale.gen"
	desc = (locale ++ " locale generated")
	ensureAvailable :: Property DebianLike
	ensureAvailable = property' desc $ \w -> do
		locales <- lines <$> (liftIO $ readFile f)
		if locale `presentIn` locales
			then ensureProperty w $
				fileProperty desc (foldr uncomment []) f
					`onChange` regenerate
			else error $ "locale " ++ locale ++ " is not present in /etc/locale.gen, even in commented out form; cannot generate"
	ensureUnavailable :: Property DebianLike
	ensureUnavailable = tightenTargets $ 
		fileProperty (locale ++ " locale not generated") (foldr comment []) f
		`onChange` regenerate

	uncomment l ls =
		if ("# " ++ locale) `isPrefixOf` l
		then drop 2 l : ls
		else l:ls
	comment l ls =
		if locale `isPrefixOf` l
		then ("# " ++ l) : ls
		else l:ls

	l `presentIn` ls = any (l `isPrefix`) ls
	l `isPrefix` x = (l `isPrefixOf` x) || (("# " ++ l) `isPrefixOf` x)

	regenerate = cmdProperty "locale-gen" []
		`assume` MadeChange