-- |
module DebianSpec where

import Test.Hspec
import Text.RawString.QQ (r)

import Text.Trifecta
import WSHS.Wrappers.Debian.Snap as Snap
import Data.ByteString
import Text.Trifecta (Result(Success, Failure ))

snapListOutput :: ByteString
snapListOutput = [r|Name   Version  Rev   Tracking       Publisher   Notes
emacs  27.2-rc1     1439  latest/stable  alexmurray  classic
emacs2  27.2     1439  latest/stable  alexmurray  classic
|]

spec :: Spec
spec = do
  describe "Debian" $ do
    it "parses the thing" $ do
      case parseByteString Snap.snapListCommandOutputParser mempty snapListOutput of
        Success s -> s `shouldBe` [ Snap "emacs" "27.2-rc1", Snap "emacs2" "27.2" ]
        Failure info -> error $ show info
