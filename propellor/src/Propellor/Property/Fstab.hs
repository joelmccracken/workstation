module Propellor.Property.Fstab (
	FsType,
	Source,
	MountPoint,
	MountOpts(..),
	module Propellor.Property.Fstab,
) where

import Propellor.Base
import qualified Propellor.Property.File as File
import Propellor.Property.Mount

import Data.Char
import Data.List
import Utility.Table

-- | Ensures that </etc/fstab> contains a line mounting the specified
-- `Source` on the specified `MountPoint`, and that it's currently mounted.
--
-- For example:
--
-- > mounted "auto" "/dev/sdb1" "/srv" mempty
--
-- Note that if anything else is already mounted at the `MountPoint`, it
-- will be left as-is by this property.
mounted :: FsType -> Source -> MountPoint -> MountOpts -> Property Linux
mounted fs src mnt opts = tightenTargets $ 
	listed fs src mnt opts
		`before` mountnow
		`requires` File.dirExists mnt
  where
	-- This use of mountPoints, which is linux-only, is why this
	-- property currently only supports linux.
	mountnow = check (notElem mnt <$> mountPoints) $
		cmdProperty "mount" [mnt]

-- | Ensures that </etc/fstab> contains a line mounting the specified
-- `Source` on the specified `MountPoint`. Does not ensure that it's
-- currently `mounted`.
listed :: FsType -> Source -> MountPoint -> MountOpts -> Property UnixLike
listed fs src mnt opts = "/etc/fstab" `File.containsLine` l
	`describe` (mnt ++ " mounted by fstab")
  where
	l = intercalate "\t" [src, mnt, fs, formatMountOpts opts, dump, passno]
	dump = "0"
	passno = "2"

-- | Ensures that </etc/fstab> contains a line enabling the specified
-- `Source` to be used as swap space, and that it's enabled.
swap :: Source -> Property Linux
swap src = listed "swap" src "none" mempty
	`onChange` swapOn src

newtype SwapPartition = SwapPartition FilePath

-- | Replaces </etc/fstab> with a file that should cause the currently
-- mounted partitions to be re-mounted the same way on boot.
--
-- For each specified MountPoint, the UUID of each partition
-- (or if there is no UUID, its label), its filesystem type,
-- and its mount options are all automatically probed.
--
-- The SwapPartitions are also included in the generated fstab.
fstabbed :: [MountPoint] -> [SwapPartition] -> Property Linux
fstabbed mnts swaps = property' "fstabbed" $ \o -> do
	fstab <- liftIO $ genFstab mnts swaps id
	ensureProperty o $ 
		"/etc/fstab" `File.hasContent` fstab

genFstab :: [MountPoint] -> [SwapPartition] -> (MountPoint -> MountPoint) -> IO [String]
genFstab mnts swaps mnttransform = do
	fstab <- liftIO $ mapM getcfg (sort mnts)
	swapfstab <- liftIO $ mapM getswapcfg swaps
	return $ header ++ formatTable (legend : fstab ++ swapfstab)
  where
	header =
		[ "# /etc/fstab: static file system information. See fstab(5)"
		, "# "
		]
	legend = ["# <file system>", "<mount point>", "<type>", "<options>", "<dump>", "<pass>"]
	getcfg mnt = sequence
		[ fromMaybe (error $ "unable to find mount source for " ++ mnt)
			<$> getM (\a -> a mnt)
				[ uuidprefix getMountUUID
				, sourceprefix getMountLabel
				, getMountSource
				]
		, pure (mnttransform mnt)
		, fromMaybe "auto" <$> getFsType mnt
		, formatMountOpts <$> getFsMountOpts mnt
		, pure "0"
		, pure (if mnt == "/" then "1" else "2")
		]
	getswapcfg (SwapPartition s) = sequence
		[ fromMaybe s <$> getM (\a -> a s)
			[ uuidprefix getSourceUUID
			, sourceprefix getSourceLabel
			]
		, pure "none"
		, pure "swap"
		, pure (formatMountOpts mempty)
		, pure "0"
		, pure "0"
		]
	prefix s getter m = fmap (s ++) <$> getter m
	uuidprefix = prefix "UUID="
	sourceprefix = prefix "LABEL="

-- | Checks if </etc/fstab> is not configured. 
-- This is the case if it doesn't exist, or
-- consists entirely of blank lines or comments.
--
-- So, if you want to only replace the fstab once, and then never touch it
-- again, allowing local modifications:
--
-- > check noFstab (fstabbed mnts [])
noFstab :: IO Bool
noFstab = ifM (doesFileExist "/etc/fstab")
	( null . filter iscfg . lines <$> readFile "/etc/fstab"
	, return True
	)
  where
	iscfg l
		| null l = False
		| otherwise = not $ "#" `isPrefixOf` dropWhile isSpace l
