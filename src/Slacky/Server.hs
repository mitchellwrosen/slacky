module Slacky.Server
  ( ServerState
  , newServerState
  , runDomainSocket
  , slackyServer
  ) where

import Slacky.Message
import Slacky.Prelude

import Data.IORef
import Data.Text.Encoding       (decodeUtf8)
import Data.Text.Lazy.Encoding  (encodeUtf8)
import Data.Text.Format
import System.Posix.Files       (removeLink)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.List      as List
import qualified Data.Map       as Map
import qualified Network.Socket as Socket

data ServerState = ServerState
  { dequeue     :: IO (Maybe Message)
  , lastChan    :: IORef (Maybe ChannelId)
  , send        :: LText -> IO ()
  , names       :: Map Text Text
  }

newServerState
  :: IO (Maybe Message) -> (LText -> IO ()) -> Map Text Text -> IO ServerState
newServerState dequeue sendMsg names = do
  lastChan <- newIORef Nothing
  pure (ServerState dequeue lastChan sendMsg names)

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
slackyServer ServerState{..} req respond
  | requestMethod req == "GET" =
      dequeue >>= \case
        Nothing -> respond (responseLBS status200 [] "")
        Just Message{..} -> do
          atomicWriteIORef lastChan (Just msgChannel)
          respond (responseLBS status200 [] msg')
         where
          chan, user :: Text
          chan = fromMaybe "???" (Map.lookup msgChannel names)
          user = fromMaybe "???" (Map.lookup msgUser    names)

          msg' :: LByteString
          msg' = encodeUtf8 (format "[{}] {}: {}\n" (chan, user, msgText))

  | requestMethod req == "POST" =
      case List.lookup "text" (queryString req) of
        Just (Just msg) ->
          readIORef lastChan >>= \case
            Nothing ->
              respond (responseLBS status400 [] "no message to reply to")
            Just chan -> do
              send
                (format
                  "{\"id\":0,\"type\":\"message\",\"channel\":\"{}\",\"text\":\"{}\"}"
                  (chan, decodeUtf8 msg))
              respond (responseLBS status200 [] "")
        _ -> respond (responseLBS status400 [] "missing text param")

  | otherwise = respond (responseLBS status405 [] "")
