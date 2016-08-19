module Slack.Types.IMSpec where

import Slack.Types.IM

-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson
import Data.Aeson.Types (parseMaybe)

-- https://www.stackage.org/lts-6.11/package/aeson-qq
import Data.Aeson.QQ (aesonQQ)

-- https://www.stackage.org/lts-6.11/package/hspec
import Test.Hspec

spec :: Spec
spec =
  describe "parseJSON" $ do
    it "succeeds on valid input" $ do
      parse [aesonQQ| { "id": "foo", "user": "bar" } |]
        `shouldBe` Just im { imId = "foo", imUser = "bar" }

    it "fails on invalid input" $ do
      parse [aesonQQ| { "id": "foo" } |]
        `shouldBe` Nothing

im :: IM
im = IM "" ""

parse :: Value -> Maybe IM
parse = parseMaybe parseJSON
