module Propellor.Property.ConfFile (
	-- * Generic conffiles with sections
	SectionStart,
	SectionPast,
	AdjustSection,
	InsertSection,
	adjustSection,
	-- * Windows .ini files
	IniSection,
	IniKey,
	containsIniSetting,
	lacksIniSetting,
	hasIniSection,
	lacksIniSection,
	iniFileContains,
	-- * Conffiles that are actually shell scripts setting env vars
	ShellKey,
	containsShellSetting,
	lacksShellSetting,
) where

import Propellor.Base
import Propellor.Property.File

import Data.List (isPrefixOf, foldl')

-- | find the line that is the start of the wanted section (eg, == "<Foo>")
type SectionStart  = Line -> Bool
-- | find a line that indicates we are past the section
-- (eg, a new section header)
type SectionPast   = Line -> Bool
-- | run on all lines in the section, including the SectionStart line;
-- can add, delete, and modify lines, or even delete entire section
type AdjustSection = [Line] -> [Line]
-- | if SectionStart does not find the section in the file, this is used to
-- insert the section somewhere within it
type InsertSection = [Line] -> [Line]

-- | Adjusts a section of conffile.
adjustSection
	:: Desc
	-> SectionStart
	-> SectionPast
	-> AdjustSection
	-> InsertSection
	-> FilePath
	-> Property UnixLike
adjustSection desc start past adjust insert = fileProperty desc go
  where
	go ls = let (pre, wanted, post) = foldl' find ([], [], []) ls
		in if null wanted
			then insert ls
			else pre ++ adjust wanted ++ post
	find (pre, wanted, post) l
		| null wanted && null post && (not . start) l =
			(pre ++ [l], wanted, post)
		| (start l && null wanted && null post)
		  || ((not . null) wanted && null post && (not . past) l) =
			  (pre, wanted ++ [l], post)
		| otherwise = (pre, wanted, post ++ [l])

-- | Name of a section of an .ini file. This value is put
-- in square braces to generate the section header.
type IniSection = String

-- | Name of a configuration setting within a .ini file.
type IniKey = String

iniHeader :: IniSection -> String
iniHeader header = '[' : header ++ "]"

adjustIniSection
	:: Desc
	-> IniSection
	-> AdjustSection
	-> InsertSection
	-> FilePath
	-> Property UnixLike
adjustIniSection desc header =
	adjustSection
	desc
	(== iniHeader header)
	("[" `isPrefixOf`)

-- | Ensures that a .ini file exists and contains a section
-- with a key=value setting.
containsIniSetting :: FilePath -> (IniSection, IniKey, String) -> Property UnixLike
containsIniSetting f (header, key, value) = adjustIniSection
	(f ++ " section [" ++ header ++ "] contains " ++ key ++ "=" ++ value)
	header
	go
	(++ [confheader, confline]) -- add missing section at end
	f
  where
	confheader = iniHeader header
	confline   = key ++ "=" ++ value
	go []      = [confline]
	go (l:ls)  = if isKeyVal l then confline : ls else l : go ls
	isKeyVal x = (filter (/= ' ') . takeWhile (/= '=')) x `elem` [key, '#':key]

-- | Removes a key=value setting from a section of an .ini file.
-- Note that the section heading is left in the file, so this is not a
-- perfect reversion of containsIniSetting.
lacksIniSetting :: FilePath -> (IniSection, IniKey, String) -> Property UnixLike
lacksIniSetting f (header, key, value) = adjustIniSection
	(f ++ " section [" ++ header ++ "] lacks " ++ key ++ "=" ++ value)
	header
	(filter (/= confline))
	id
	f
  where
	confline = key ++ "=" ++ value

-- | Ensures that a .ini file exists and contains a section
-- with a given key=value list of settings.
hasIniSection :: FilePath -> IniSection -> [(IniKey, String)] -> Property UnixLike
hasIniSection f header keyvalues = adjustIniSection
	("set " ++ f ++ " section [" ++ header ++ "]")
	header
	go
	(++ confheader : conflines) -- add missing section at end
	f
  where
	confheader = iniHeader header
	conflines  = map (\(key, value) -> key ++ "=" ++ value) keyvalues
	go _       = confheader : conflines

-- | Ensures that a .ini file does not contain the specified section.
lacksIniSection :: FilePath -> IniSection -> Property UnixLike
lacksIniSection f header = adjustIniSection
	(f ++ " lacks section [" ++ header ++ "]")
	header
	(const []) -- remove all lines of section
	id -- add no lines if section is missing
	f

-- | Specifies the whole content of a .ini file.
--
-- Revertijg this causes the file not to exist.
iniFileContains :: FilePath -> [(IniSection, [(IniKey, String)])] -> RevertableProperty UnixLike UnixLike
iniFileContains f l = f `hasContent` content <!> notPresent f
  where
	content = concatMap sectioncontent l
	sectioncontent (section, keyvalues) = iniHeader section :
		map (\(key, value) -> key ++ "=" ++ value) keyvalues

-- | Key for a shell conffile property.  Conventionally uppercase letters and
-- numbers with underscores for separators.  See files in </etc/default>.
type ShellKey = String

-- | Ensures a shell conffile (like those in </etc/default>) exists and has a
-- key=value pair.
--
-- Comments out any further settings of that key further down the
-- file, to avoid those taking precedence.
containsShellSetting :: FilePath -> (ShellKey, String) -> Property UnixLike
containsShellSetting f (k, v) = adjust `before` dedup
  where
	adjust = adjustSection
		(f ++ " contains " ++ k ++ "=" ++ v)
		isline
		(not . isline)
		(const [line])
		(++ [line])
		f
	dedup = fileProperty "" dedup' f
	dedup' ls = let (pre, wanted, post) = foldl' find ([], [], []) ls
		    in pre ++ wanted ++ map commentIfIsline post
	find (pre, wanted, post) l
		| null wanted && (not . isline) l = (pre ++ [l], wanted, post)
		| null wanted && isline l         = (pre, [l], post)
		| otherwise                       = (pre, wanted, post ++ [l])
	-- some /etc/default files comment settings lines with '# '
	-- and some use '#'; one advantage of just using '#' is that
	-- it distinguishes settings values from prose comments
	commentIfIsline l
		| isline l  = '#':l
		| otherwise = l

	isline s = (k ++ "=") `isPrefixOf` s
	line = k ++ "=" ++ shellEscape v

-- | Comments out a key=value pair in a shell conffile.
--
-- Does not delete the file if empty, and does not uncomment any
-- lines, so not a perfect reversion of 'containsShellSetting'.
lacksShellSetting :: FilePath -> (ShellKey, String) -> Property UnixLike
lacksShellSetting f (k, v) =
	fileProperty (f ++ "lacks shell setting " ++ k ++ "=" ++ v) go f
  where
	go ls = map commentOut ls
	commentOut l
		| (k ++ "=") `isPrefixOf` l = '#':l
		| otherwise                 = l
