module Slacky.Message where

import Data.Text (Text)

type ChannelId = Text
type UserId    = Text

data Message = Message
  { msgChannel :: ChannelId
  , msgUser    :: UserId
  , msgText    :: Text
  }
