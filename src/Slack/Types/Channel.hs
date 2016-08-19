module Slack.Types.Channel where

import Slacky.Prelude

-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson

type ChannelId   = Text
type ChannelName = Text

data Channel = Channel
  { channelId   :: ChannelId
  , channelName :: ChannelName
  } deriving (Eq, Show)

-- To test only this instance:
--
--     stack test --fast --file-watch --test-arguments="-m Slack.Types.Channel"
--
instance FromJSON Channel where
  parseJSON = implementMe
