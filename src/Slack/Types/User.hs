module Slack.Types.User where

import Slacky.Prelude

-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson

type UserId   = Text
type UserName = Text

data User = User
  { userId   :: Text
  , userName :: Text
  } deriving (Eq, Show)

-- To test only this instance:
--
--     stack test --fast --file-watch --test-arguments="-m Slack.Types.User"
--
instance FromJSON User where
  parseJSON = implementMe
