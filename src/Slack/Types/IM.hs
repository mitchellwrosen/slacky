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

instance FromJSON IM where
  parseJSON =
    withObject "object" $ \o -> do
      ident <- o .: "id"
      user  <- o .: "user"
      pure (IM ident user)
