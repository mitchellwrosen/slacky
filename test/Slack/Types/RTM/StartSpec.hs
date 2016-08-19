module Slack.Types.RTM.StartSpec where

import Slack.Types.Channel
import Slack.Types.IM
import Slack.Types.RTM.Start
import Slack.Types.User

-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson
import Data.Aeson.Types (parseMaybe)

-- https://www.stackage.org/lts-6.11/package/aeson-qq
import Data.Aeson.QQ (aesonQQ)

-- https://www.stackage.org/lts-6.11/package/hspec
import Test.Hspec

spec :: Spec
spec = do
  describe "parseHostPath" $
    it "parses host and path" $ do
      parseHostPath "http://www.host.com:80/path"
        `shouldBe` Just ("www.host.com", "/path")

      parseHostPath "http://www.host.com:80"
        `shouldBe` Just ("www.host.com", "")

  describe "parseJSON" $ do
    it "succeeds on valid input" $ do
      parse [aesonQQ|
          {
            "ok": true,
            "url": "http://www.host.com:80/path",
            "users": [{"id": "foo", "name": "bar"}],
            "channels": [{"id": "foo", "name": "bar"}],
            "groups": [{"id": "foo", "name": "bar"}],
            "ims": [{"id": "foo", "user": "bar"}]
          }
        |]
        `shouldBe`
          Just rtmStart
            { rtmStartHost     = "www.host.com"
            , rtmStartPath     = "/path"
            , rtmStartUsers    = [User "foo" "bar"]
            , rtmStartChannels = [Channel "foo" "bar"]
            , rtmStartGroups   = [Channel "foo" "bar"]
            , rtmStartIMs      = [IM "foo" "bar"]
            }

    it "fails on invalid input" $ do
      parse [aesonQQ|
          {
            "ok": false,
            "url": "www.google.com",
            "users": [{"id": "foo", "name": "bar"}],
            "channels": [{"id": "foo", "name": "bar"}],
            "groups": [{"id": "foo", "name": "bar"}],
            "ims": [{"id": "foo", "user": "bar"}]
          }
        |]
        `shouldBe` Nothing

rtmStart :: RTMStart
rtmStart = RTMStart "" "" [] [] [] []

parse :: Value -> Maybe RTMStart
parse = parseMaybe parseJSON
