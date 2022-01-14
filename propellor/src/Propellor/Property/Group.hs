module Propellor.Property.Group where

import Propellor.Base
import Propellor.Property.User (hasGroup)

type GID = Int

exists :: Group -> Maybe GID -> Property UnixLike
exists (Group group') mgid = check test (cmdProperty "addgroup" (args mgid))
	`describe` unwords ["group", group']
  where
	groupFile = "/etc/group"
	test = not . elem group' . words <$> readProcess "cut" ["-d:", "-f1", groupFile]
	args Nothing = [group']
	args (Just gid) = ["--gid", show gid, group']

hasUser :: Group -> User -> Property DebianLike
hasUser = flip hasGroup
