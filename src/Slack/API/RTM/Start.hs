-- | A simple interface to the "rtm.start" web API call:
--
--     https://api.slack.com/methods/rtm.start
--
-- Given an API token, get back a wss:// to connect to and some metadata.
--
-- We don't want most of this data, so just return an 'RtmInfo' blob with the
-- stuff we care about.
module Slack.API.RTM.Start
  ( RtmInfo(..)
  , ApiToken
  , rtmStart
  ) where

import Slacky.Prelude

import Data.Aeson
import Data.Aeson.Types  (Parser)
import Data.Text         (unpack)
import Lens.Micro
import Lens.Micro.Extras
import Network.URI       (URI(..), URIAuth(..), parseURI)
import Network.Wreq
import System.IO
import System.Exit
import Web.Slack

import qualified Data.Map as Map


type ApiToken = Text

data RtmInfo = RtmInfo
  { rtmHost  :: String
    -- ^ The host to connect to.
  , rtmPath  :: String
    -- ^ The port to connect to.
  , rtmNames :: Map Text Text
    -- ^ Mapping from IDs to name (e.g. "U12345678" to "Jon")
  } deriving Show

instance FromJSON RtmInfo where
  parseJSON =
    withObject "object" $ \o -> do
      True         <- o .: "ok"
      url          <- o .: "url"
      (host, path) <- parseHostPath url
      users        <- o .: "users"
      channels     <- o .: "channels"
      groups       <- o .: "groups"
      ims          <- o .: "ims"

      let users', channels', groups', ims' :: Map Text Text
          users'    = foldr insertUser        mempty (users    :: [User])
          channels' = foldr insertChannel     mempty (channels :: [Channel])
          groups'   = foldr insertChannel     mempty (groups   :: [Channel])
          ims'      = foldr (insertIM users') mempty (ims      :: [IM])

          names :: Map Text Text
          names = users' <> channels' <> groups' <> ims'

      pure (RtmInfo host path names)
   where
    parseHostPath :: Text -> Parser (String, String)
    parseHostPath raw =
      case parseURI (unpack raw) of
        Nothing  -> fail "Couldn't parse URI"
        Just uri ->
          case uriAuthority uri of
            Nothing -> fail "Couldn't parse URI"
            Just uri_auth ->
              pure (uriRegName uri_auth, uriPath uri)

    insertUser :: User -> Map Text Text -> Map Text Text
    insertUser u = Map.insert (_getId (_userId u)) (_userName u)

    insertChannel :: Channel -> Map Text Text -> Map Text Text
    insertChannel c = Map.insert (_getId (_channelId c)) (_channelName c)

    insertIM :: Map Text Text -> IM -> Map Text Text -> Map Text Text
    insertIM users im = Map.insert (_getId (_imId im)) uname
     where
      uname :: Text
      uname = fromMaybe "???" (Map.lookup (_getId (_imUser im)) users)


rtmStart :: ApiToken -> IO RtmInfo
rtmStart token = do
  resp <- getWith opts "https://slack.com/api/rtm.start"
  case decode (view responseBody resp) of
    Nothing -> do
      hPutStrLn stderr "Failed to decode slack response"
      exitWith (ExitFailure 1)
    Just info -> pure info
 where
  opts :: Options
  opts =
    defaults
      & set (param "token")         [token]
      & set (param "simple_latest") []
      & set (param "no_unreads")    []
