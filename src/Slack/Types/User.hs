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

instance FromJSON User where
  parseJSON =
    withObject "object" $ \o -> do
      ident <- o .: "id"
      name  <- o .: "name"
      pure (User ident name)
