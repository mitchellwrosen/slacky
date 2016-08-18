module Slacky.Server
  ( ServerState
  , newServerState
  , runDomainSocket
  , slackyServer
  ) where

import Slacky.LoggerT
import Slacky.Message
import Slacky.Prelude

import Control.Monad.Trans.Unlift
import Data.IORef
import System.Posix.Files         (removeLink)
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

runDomainSocket
  :: (MonadLogger m, MonadMask m, MonadIO m, MonadBaseUnlift IO m)
  => FilePath
  -> (Request -> (Response -> m ResponseReceived) -> m ResponseReceived) -> m ()
runDomainSocket sockfile app =
  bracket
    (io (Socket.socket Socket.AF_UNIX Socket.Stream 0))
    (io . Socket.close)
    (\sock -> do
      log Debug ("Binding server to " <> pack sockfile)
      bracket_
        (io (Socket.bind sock (Socket.SockAddrUnix sockfile)))
        (tryAny (io (removeLink sockfile)))
        (do
          log Debug ("Server listening on " <> pack sockfile)
          io (Socket.listen sock Socket.maxListenQueue)

          app' <- unliftApp app
          io (runSettingsSocket defaultSettings sock app')))

unliftApp
  :: (MonadIO m, MonadBaseUnlift IO m)
  => (Request -> (Response -> m ResponseReceived) -> m ResponseReceived)
  -> m Application
unliftApp app = do
  unlift <- askRunBase
  pure (\req respond -> unlift (app req (io . respond)))

slackyServer
  :: (MonadLogger m, MonadIO m)
  => ServerState -> Request -> (Response -> m ResponseReceived)
  -> m ResponseReceived
slackyServer state req respond =
  case pathInfo req of
    [] -> homeR state req respond
    _  -> respond response404

-- /
homeR
  :: (MonadLogger m, MonadIO m)
  => ServerState -> Request -> (Response -> m ResponseReceived)
  -> m ResponseReceived
homeR state req respond = do
  log Debug ("Server received: " <> show req)

  if | requestMethod req == "GET"  -> getHomeR  state req respond
     | requestMethod req == "POST" -> postHomeR state req respond
     | otherwise                   -> respond response405

-- GET /
--
-- Get all messages received since the last GET.
getHomeR
  :: (MonadLogger m, MonadIO m)
  => ServerState -> Request -> (Response -> m ResponseReceived)
  -> m ResponseReceived
getHomeR ServerState{..} _ respond =
  io dequeue >>= \case
    [] -> respond (responseLBS status200 [] "")
    (m:ms) -> do
      io (atomicWriteIORef lastChan (Just (msgChannel m)))

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
postHomeR
  :: forall m.
     (MonadLogger m, MonadIO m)
  => ServerState -> Request -> (Response -> m ResponseReceived)
  -> m ResponseReceived
postHomeR ServerState{..} req respond =
  case List.lookup "text" query of
    Just (Just msg) ->
      case List.lookup "channel" query of
        Just (Just chan) ->
          case Map.lookup (decodeUtf8 chan) ids of
            Nothing -> respond (responseLBS status400 [] "no such channel")
            Just chan_id -> sendMessage msg chan_id

        _ ->
          io (readIORef lastChan) >>= \case
            Nothing ->
              respond (responseLBS status400 [] "no message to reply to")
            Just chan_id -> sendMessage msg chan_id

    _ -> respond (responseLBS status400 [] "missing text param")
 where
  sendMessage :: ByteString -> ChannelId -> m ResponseReceived
  sendMessage msg chan_id = do
    let json :: LText
        json =
          format
            "{\"id\":0,\"type\":\"message\",\"channel\":\"{}\",\"text\":\"{}\"}"
            (chan_id, decodeUtf8 msg)

    log Debug ("Server sending: " <> json)
    io (send json)

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
