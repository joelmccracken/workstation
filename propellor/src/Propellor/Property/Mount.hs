{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

-- | Properties in this module ensure that things are currently mounted,
-- but without making the mount persistent. Use `Propellor.Property.Fstab`
-- to configure persistent mounts.

module Propellor.Property.Mount where

import Propellor.Base
import Utility.Path

import Data.List
import qualified Data.Semigroup as Sem

-- | type of filesystem to mount ("auto" to autodetect)
type FsType = String

-- | A device or other thing to be mounted.
type Source = String

-- | A mount point for a filesystem.
type MountPoint = FilePath

-- | Filesystem mount options. Eg, MountOpts ["errors=remount-ro"]
--
-- For default mount options, use `mempty`.
newtype MountOpts = MountOpts [String]
	deriving (Sem.Semigroup, Monoid)

class ToMountOpts a where
	toMountOpts :: a -> MountOpts
	
instance ToMountOpts MountOpts where
	toMountOpts = id

instance ToMountOpts String where
	toMountOpts s = MountOpts [s]

formatMountOpts :: MountOpts -> String
formatMountOpts (MountOpts []) = "defaults"
formatMountOpts (MountOpts l) = intercalate "," l

-- | Mounts a device, without listing it in </etc/fstab>.
--
-- Note that this property will fail if the device is already mounted
-- at the MountPoint.
mounted :: FsType -> Source -> MountPoint -> MountOpts -> Property UnixLike
mounted fs src mnt opts = property (mnt ++ " mounted") $ 
	toResult <$> liftIO (mount fs src mnt opts)

-- | Bind mounts the first directory so its contents also appear
-- in the second directory.
bindMount :: FilePath -> FilePath -> Property Linux
bindMount src dest = tightenTargets $
	cmdProperty "mount" ["--bind", src, dest]
		`assume` MadeChange
		`describe` ("bind mounted " ++ src ++ " to " ++ dest)

-- | Enables swapping to a device, which must be formatted already as a swap
-- partition.
swapOn :: Source -> RevertableProperty Linux Linux
swapOn mnt = tightenTargets doswapon <!> tightenTargets doswapoff
  where
	swaps = lines <$> readProcess "swapon" ["--show=NAME"]
	doswapon = check (notElem mnt <$> swaps) $
		cmdProperty "swapon" [mnt]
	doswapoff = check (elem mnt <$> swaps) $
		cmdProperty "swapoff" [mnt]

mount :: FsType -> Source -> MountPoint -> MountOpts -> IO Bool
mount fs src mnt opts = boolSystem "mount" $
	[ Param "-t", Param fs
	, Param "-o", Param (formatMountOpts opts)
	, Param src
	, Param mnt
	]

-- | Lists all mount points of the system.
mountPoints :: IO [MountPoint]
mountPoints = lines <$> readProcess "findmnt" ["-rn", "--output", "target"]

-- | Checks if anything is mounted at the MountPoint.
isMounted :: MountPoint -> IO Bool
isMounted mnt = isJust <$> getFsType mnt

-- | Finds all filesystems mounted inside the specified directory.
mountPointsBelow :: FilePath -> IO [MountPoint]
mountPointsBelow target = filter (\p -> simplifyPath p /= simplifyPath target)
	. filter (dirContains target)
	<$> mountPoints

-- | Get mountpoints which are bind mounts of subdirectories of mounted
-- filesystems
--
-- E.g. as created by @mount --bind /etc/foo /etc/bar@ where @/etc/foo@ is not
-- itself a mount point, but just a subdirectory.  These are sometimes known as
-- "partial bind mounts"
partialBindMountsOf :: FilePath -> IO [MountPoint]
partialBindMountsOf sourceDir =
	map (drop 2 . dropWhile (/= ']')) . filter getThem . lines
	<$> readProcess "findmnt" ["-rn", "--output", "source,target"]
  where
	getThem l = bracketed `isSuffixOf` (takeWhile (/= ' ') l)
	bracketed = "[" ++ sourceDir ++ "]"

-- | Filesystem type mounted at a given location.
getFsType :: MountPoint -> IO (Maybe FsType)
getFsType p = findmntField "fstype" [p]

-- | Mount options for the filesystem mounted at a given location.
getFsMountOpts :: MountPoint -> IO MountOpts
getFsMountOpts p = maybe mempty toMountOpts
	<$> findmntField "fs-options" [p]

type UUID = String

-- | UUID of filesystem mounted at a given location.
getMountUUID :: MountPoint -> IO (Maybe UUID)
getMountUUID p = findmntField "uuid" [p]

-- | UUID of a device
getSourceUUID :: Source -> IO (Maybe UUID)
getSourceUUID = blkidTag "UUID"

type Label = String

-- | Label of filesystem mounted at a given location.
getMountLabel :: MountPoint -> IO (Maybe Label)
getMountLabel p = findmntField "label" [p]

-- | Label of a device
getSourceLabel :: Source -> IO (Maybe UUID)
getSourceLabel = blkidTag "LABEL"

-- | Device mounted at a given location.
getMountSource :: MountPoint -> IO (Maybe Source)
getMountSource p = findmntField "source" [p]

-- | Device that a given path is located within.
getMountContaining :: FilePath -> IO (Maybe Source)
getMountContaining p = findmntField "source" ["-T", p]

findmntField :: String -> [String] -> IO (Maybe String)
findmntField field ps = catchDefaultIO Nothing $
	headMaybe . filter (not . null) . lines
		<$> readProcess "findmnt" ("-n" : ps ++ ["--output", field])

blkidTag :: String -> Source -> IO (Maybe String)
blkidTag tag dev = catchDefaultIO Nothing $
	headMaybe . filter (not . null) . lines
		<$> readProcess "blkid" [dev, "-s", tag, "-o", "value"]

-- | Unmounts a device or mountpoint,
-- lazily so any running processes don't block it.
--
-- Note that this will fail if it's not mounted.
umountLazy :: FilePath -> IO ()
umountLazy mnt =  
	unlessM (boolSystem "umount" [ Param "-l", Param mnt ]) $
		stopPropellorMessage $ "failed unmounting " ++ mnt

-- | Unmounts anything mounted inside the specified directory,
-- not including the directory itself.
unmountBelow :: FilePath -> IO ()
unmountBelow d = do
	submnts <- mountPointsBelow d
	-- sort so sub-mounts are unmounted before the mount point
	-- containing them
	forM_ (reverse (sort submnts)) umountLazy
