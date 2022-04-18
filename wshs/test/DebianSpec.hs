-- |
module DebianSpec where

import RIO

import Test.Hspec
import Text.RawString.QQ (r)

import Text.Trifecta
import WSHS.Wrappers.Debian.Snap as Snap

snapListOutput :: ByteString
snapListOutput = [r|
Name    Version        Rev    Tracking       Publisher   Notes
emacs  27.2-rc1     1439  latest/stable  alexmurray  classic
emacs2  27.2     1439  latest/stable  alexmurray  classic
|]

-- came from:
--  https://github.com/joelmccracken/workstation/runs/5962404131
-- + ./result/bin/ws install -m ci-ubuntu
-- ws: ErrInfo {_errDoc = (interactive):1:1: error: expected: "Name"
-- 1 |   snapd
--   | ^       , _errDeltas = [Columns 0 0]}; full text that did not parse:   snapd
-- Name    Version        Rev    Tracking       Publisher   Notes
-- core20  20220318       1405   latest/stable  canonical*  base
-- lxd     4.0.9-8e2046b  22753  4.0/stable/…   canonical*  -
-- snapd   2.55.2         15314  latest/stable  canonical*
-- CallStack (from HasCallStack):
--   error, called at src/WSHS/Properties/Debian.hs:26:15 in wshs-0.1.0.0-FuOPoYWybRuIZN0TuKQVgX:WSHS.Properties.Debian
-- real	0m0.406s
-- user	0m0.023s
-- sys	0m0.297s

snapListOutputCI :: ByteString
snapListOutputCI = [r|  snapd
Name    Version        Rev    Tracking       Publisher   Notes
core20  20220318       1405   latest/stable  canonical*  base
lxd     4.0.9-8e2046b  22753  4.0/stable/…   canonical*  -
snapd   2.55.2         15314  latest/stable  canonical*
|]

snapListOutputCI2 :: ByteString
snapListOutputCI2 = [r|Name    Version        Rev    Tracking         Publisher   Notes
core20  20220318       1405   latest/stable    canonical✓  base
emacs   28.1           1490   latest/stable    alexmurray  classic
lxd     5.0.0-e478009  22894  latest/stable/…  canonical✓  -
snapd   2.54.4         15177  latest/stable    canonical✓  snapd
|]

snapListOutputCI3 :: ByteString
snapListOutputCI3 = [r|snapd  canonical*    latest/stable  15177         2.54.4   snapd
-  canonical*  latest/stable/…  22894  5.0.0-e478009     lxd
classic  alexmurray    latest/stable   1490           28.1   emacs
base  canonical*    latest/stable   1405       20220318  core20
Notes   Publisher         Tracking    Rev        Version    Name
|]



spec :: Spec
spec = do
  describe "Debian" $ do
    describe "Snap List command" $ do
      it "parses output from my ubuntu server" $ do
        case parseByteString Snap.snapListCommandOutputParser mempty snapListOutput of
          Success s -> s `shouldBe` [ Snap "emacs" "27.2-rc1", Snap "emacs2" "27.2" ]
          Failure info -> error $ show info

      it "parses output from github ci" $ do
        case parseByteString Snap.snapListCommandOutputParser mempty snapListOutputCI of
          Success s -> s `shouldBe`
            [ Snap {name = "core20", version = "20220318"}
            , Snap {name = "lxd", version = "4.0.9-8e2046b"}
            , Snap {name = "snapd", version = "2.55.2"}
            ]

          Failure info -> error $ show info

        case parseByteString Snap.snapListCommandOutputParser mempty snapListOutputCI2 of
          Success s -> s `shouldBe`
            [ Snap {name = "core20", version = "20220318"}
            , Snap {name = "emacs", version = "28.1"}
            , Snap {name = "lxd", version = "5.0.0-e478009"}
            , Snap {name = "snapd", version = "2.54.4"}
            ]
          Failure info -> error $ show info

        case parseByteString Snap.snapListCommandOutputParser mempty snapListOutputCI3 of
          Success s -> s `shouldBe`
            [ Snap {name = "core20", version = "20220318"}
            , Snap {name = "emacs", version = "28.1"}
            , Snap {name = "lxd", version = "5.0.0-e478009"}
            , Snap {name = "snapd", version = "2.54.4"}
            ]
          Failure info -> error $ show info
