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

instance FromJSON Channel where
  parseJSON =
    withObject "object" $ \o -> do
      ident <- o .: "id"
      name  <- o .: "name"
      pure (Channel ident name)
