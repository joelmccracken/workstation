-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Ccache (
	hasCache,
	hasLimits,
	Limit(..),
	DataSize,
) where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

import Utility.DataUnits
import System.Posix.Files
import qualified Data.Semigroup as Sem

-- | Limits on the size of a ccache
data Limit
	-- | The maximum size of the cache, as a string such as "4G"
	= MaxSize DataSize
	-- | The maximum number of files in the cache
	| MaxFiles Integer
	-- | A cache with no limit specified
	| NoLimit
	| Limit :+ Limit

instance Sem.Semigroup Limit where
	(<>) = (:+)

instance Monoid Limit where
	mempty  = NoLimit
	mappend = (Sem.<>)

-- | A string that will be parsed to get a data size.
--
-- Examples: "100 megabytes" or "0.5tb"
type DataSize = String

maxSizeParam :: DataSize -> Maybe String
maxSizeParam s = readSize dataUnits s
	>>= \sz -> Just $ "--max-size=" ++ ccacheSizeUnits sz

-- Generates size units as used in ccache.conf.  The smallest unit we can
-- specify in a ccache config files is a kilobyte
ccacheSizeUnits :: Integer -> String
ccacheSizeUnits sz = filter (/= ' ') (roughSize cfgfileunits True sz)
  where
	cfgfileunits :: [Unit]
	cfgfileunits =
	        [ Unit (p 4) "Ti" "terabyte"
		, Unit (p 3) "Gi" "gigabyte"
		, Unit (p 2) "Mi" "megabyte"
		, Unit (p 1) "Ki" "kilobyte"
		]
	p :: Integer -> Integer
	p n = 1024^n

-- | Set limits on a given ccache
hasLimits :: FilePath -> Limit -> Property DebianLike
path `hasLimits` limit = go `requires` installed
  where
	go
		| null params' = doNothing
		-- We invoke ccache itself to set the limits, so that it can
		-- handle replacing old limits in the config file, duplicates
		-- etc.
		| null errors =
			cmdPropertyEnv "ccache" params' [("CCACHE_DIR", path)]
			`changesFileContent` (path </> "ccache.conf")
		| otherwise = property "couldn't parse ccache limits" $
			errorMessage $ unlines errors

	params = limitToParams limit
	(errors, params') = partitionEithers params

limitToParams :: Limit -> [Either String String]
limitToParams NoLimit = []
limitToParams (MaxSize s) = case maxSizeParam s of
	Just param -> [Right param]
	Nothing -> [Left $ "unable to parse data size " ++ s]
limitToParams (MaxFiles f) = [Right $ "--max-files=" ++ val f]
limitToParams (l1 :+ l2) = limitToParams l1 <> limitToParams l2

-- | Configures a ccache in /var/cache for a group
--
-- If you say
--
-- > & (Group "foo") `Ccache.hasGroupCache`
-- > 	(Ccache.MaxSize "4G" <> Ccache.MaxFiles 10000)
--
-- you instruct propellor to create a ccache in /var/cache/ccache-foo owned and
-- writeable by the foo group, with a maximum cache size of 4GB or 10000 files.
hasCache :: Group -> Limit -> RevertableProperty DebianLike UnixLike
group@(Group g) `hasCache` limit = (make `requires` installed) <!> delete
  where
	make = propertyList ("ccache for " ++ g ++ " group exists") $ props
			& File.dirExists path
			& File.ownerGroup path (User "root") group
			& File.mode path (combineModes $
					readModes ++ executeModes ++
					[ ownerWriteMode
					, groupWriteMode
					, setGroupIDMode
					]) `onChange` fixSetgidBit
				-- here, we use onChange to catch upgrades from
				-- 3.0.5 where the setGroupIDMode line was not
				-- present
			& hasLimits path limit

	delete = check (doesDirectoryExist path) $
		cmdProperty "rm" ["-r", path] `assume` MadeChange
		`describe` ("ccache for " ++ g ++ " does not exist")

	-- Here we deal with a bug in Propellor 3.0.5.  If the ccache was
	-- created with that version, it will not have the setgid bit set.  That
	-- means its subdirectories won't have inherited the setgid bit, and
	-- then the files in those directories won't be owned by group sbuild.
	-- This breaks ccache.
	fixSetgidBit :: Property UnixLike
	fixSetgidBit =
		(cmdProperty "find"
			[ path
			, "-type", "d"
			, "-exec", "chmod", "g+s"
			, "{}", "+"
			] `assume` MadeChange)
		`before`
		(cmdProperty "chown"
			[ "-R"
			, "root:" ++ g
			, path
			] `assume` MadeChange)

	path = "/var/cache/ccache-" ++ g

installed :: Property DebianLike
installed = Apt.installed ["ccache"]
