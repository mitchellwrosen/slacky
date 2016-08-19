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

instance FromJSON RTMStart where
  parseJSON =
    withObject "object" $ \o -> do
      True <- o .: "ok"

      Just (host, path) <- fmap parseHostPath (o .: "url")

      users    <- o .: "users"
      channels <- o .: "channels"
      groups   <- o .: "groups"
      ims      <- o .: "ims"

      pure (RTMStart host path users channels groups ims)

-- This uses both view patterns (-XViewPatterns) and record wildcards
-- (-XRecordWildCards). I've enabled them globally in package.yaml.
parseHostPath :: Text -> Maybe (String, String)
parseHostPath (unpack -> str) = do
  URI{..}     <- parseURI str
  URIAuth{..} <- uriAuthority
  pure (uriRegName, uriPath)
