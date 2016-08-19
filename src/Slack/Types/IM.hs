module Slack.Types.IM where

import Slack.Types.User (UserId)
import Slacky.Prelude

-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson

type IMId = Text

data IM = IM
  { imId   :: IMId
  , imUser :: UserId
  } deriving (Eq, Show)

-- To test only this instance:
--
--     stack test --fast --file-watch --test-arguments="-m Slack.Types.IM"
--
instance FromJSON IM where
  parseJSON = implementMe
