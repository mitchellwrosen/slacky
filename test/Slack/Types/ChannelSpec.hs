module Slack.Types.ChannelSpec where

import Slack.Types.Channel

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
        `shouldBe` Just channel { channelId = "foo", channelName = "bar" }

    it "fails on invalid input" $ do
      parse [aesonQQ| { "id": "foo" } |]
        `shouldBe` Nothing

channel :: Channel
channel = Channel "" ""

parse :: Value -> Maybe Channel
parse = parseMaybe parseJSON
