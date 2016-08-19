module Slack.Types.UserSpec where

import Slack.Types.User

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
      parse [aesonQQ| { "id": "foo", "name": "bar" } |]
        `shouldBe` Just user { userId = "foo", userName = "bar" }

    it "fails on invalid input" $ do
      parse [aesonQQ| { "id": "foo" } |]
        `shouldBe` Nothing

user :: User
user = User "" ""

parse :: Value -> Maybe User
parse = parseMaybe parseJSON
