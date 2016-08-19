module Slack.Types.RTM.Start where

import Slack.Types.Channel
import Slack.Types.IM
import Slack.Types.User
import Slacky.Prelude

-- https://hackage.haskell.org/package/aeson
import Data.Aeson

-- https://hackage.haskell.org/package/vector
import Data.Vector (Vector)

-- https://www.stackage.org/lts-6.11/package/network-uri
import Network.URI

data RTMStart = RTMStart
  { rtmStartHost     :: String
  , rtmStartPath     :: String
  , rtmStartUsers    :: Vector User
  , rtmStartChannels :: Vector Channel
  , rtmStartGroups   :: Vector Channel
  , rtmStartIMs      :: Vector IM
  } deriving (Eq, Show)

-- Implement the FromJSON instances for User, Channel, and IM first!
--
-- To test only this instance:
--
--     stack test --fast --file-watch --test-arguments="-m Slack.Types.RTM.Start.parseJSON"
--
instance FromJSON RTMStart where
  parseJSON = implementMe

-- Parse a host/path out of a Text.
--
-- Convert from a Text to a String using 'unpack', imported from Slacky.Prelude.
-- Then, use the parsing functions in Network.URI to dig out the host and path.
--
-- To test only this function:
--
--     stack test --fast --file-watch --test-arguments="-m Slack.Types.RTM.Start.parseHostPath"
--
parseHostPath :: Text -> Maybe (String, String)
parseHostPath = implementMe
