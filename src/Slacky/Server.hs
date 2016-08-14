module Slacky.Server
  ( ServerState
  , newServerState
  , runDomainSocket
  , slackyServer
  ) where

import Slacky.Message
import Slacky.Prelude

import Data.IORef
import Data.Text.Encoding      (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Format
import System.Posix.Files      (removeLink)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.List      as List
import qualified Data.Map       as Map
import qualified Network.Socket as Socket

data ServerState = ServerState
  { dequeue  :: IO [Message]
  , lastChan :: IORef (Maybe ChannelId)
  , send     :: LText -> IO ()
  , names    :: Map Text Text -- id-to-name mapping
  , ids      :: Map Text Text -- name-to-id mapping
  }

newServerState
  :: IO [Message] -> (LText -> IO ()) -> Map Text Text -> Map Text Text
  -> IO ServerState
newServerState dequeue sendMsg names ids = do
  lastChan <- newIORef Nothing
  pure (ServerState dequeue lastChan sendMsg names ids)

runDomainSocket :: FilePath -> Application -> IO ()
runDomainSocket sockfile app =
  bracket
    (Socket.socket Socket.AF_UNIX Socket.Stream 0)
    (Socket.close)
    (\sock -> do
      bracket_
        (Socket.bind sock (Socket.SockAddrUnix sockfile))
        (tryAny (removeLink sockfile))
        (do
          Socket.listen sock Socket.maxListenQueue
          runSettingsSocket defaultSettings sock app))

slackyServer :: ServerState -> Application
slackyServer state req respond =
  case pathInfo req of
    [] -> homeR state req respond
    _  -> respond response404

-- /
homeR :: ServerState -> Application
homeR state req respond
  | requestMethod req == "GET"  = getHomeR  state req respond
  | requestMethod req == "POST" = postHomeR state req respond
  | otherwise                   = respond response405

-- GET /
--
-- Get all messages received since the last GET.
getHomeR :: ServerState -> Application
getHomeR ServerState{..} _ respond =
  dequeue >>= \case
    [] -> respond (responseLBS status200 [] "")
    (m:ms) -> do
      atomicWriteIORef lastChan (Just (msgChannel m))

      let response :: LByteString
          response = mconcat (map (formatMessage names) (reverse (m:ms)))

      respond (responseLBS status200 [] response)

-- POST /
--
--   /?text=<msg>
--
--     Send <msg> to the last-received-from channel
--
--   /?text=<msg>&channel=<chan>
--
--     Send <msg> to <chan>
postHomeR :: ServerState -> Application
postHomeR ServerState{..} req respond =
  case List.lookup "text" query of
    Just (Just msg) ->
      case List.lookup "channel" query of
        Just (Just chan) ->
          case Map.lookup (decodeUtf8 chan) ids of
            Nothing -> respond (responseLBS status400 [] "no such channel")
            Just chan_id -> sendMessage msg chan_id

        _ ->
          readIORef lastChan >>= \case
            Nothing ->
              respond (responseLBS status400 [] "no message to reply to")
            Just chan_id -> sendMessage msg chan_id

    _ -> respond (responseLBS status400 [] "missing text param")
 where
  sendMessage :: ByteString -> ChannelId -> IO ResponseReceived
  sendMessage msg chan_id = do
    send
      (format
        "{\"id\":0,\"type\":\"message\",\"channel\":\"{}\",\"text\":\"{}\"}"
        (chan_id, decodeUtf8 msg))
    respond (responseLBS status200 [] "")

  query :: Query
  query = queryString req

response404 :: Response
response404 =
  responseLBS status404 [("Content-Type", "text/plain")] "Not found"

response405 :: Response
response405 = responseLBS status405 [] ""

formatMessage :: Map Text Text -> Message -> LByteString
formatMessage names Message{..} =
  encodeUtf8 (format "[{}] {}: {}\n" (chan, user, msgText))
 where
  chan :: Text
  chan = fromMaybe "???" (Map.lookup msgChannel names)

  user :: Text
  user = fromMaybe "???" (Map.lookup msgUser names)
