module Slacky.Client
  ( ClientState
  , clientState
  , slackyClient
  ) where

import Slacky.LoggerT
import Slacky.Message
import Slacky.Prelude

import Data.Aeson

import qualified Data.ByteString.Lazy as LByteString
import qualified Web.Slack            as Slack

data ClientState = ClientState
  { enqueue :: Message -> IO ()
  , recv    :: IO LByteString
  }

clientState :: (Message -> IO ()) -> IO LByteString -> ClientState
clientState = ClientState

slackyClient :: (MonadLogger m, MonadIO m) => ClientState -> m ()
slackyClient ClientState{..} = forever $ do
  bytes <- io recv

  -- Just assume UTF-8 encoding for printing, since pringing out a hex
  -- representation seems less than ideal.
  log Debug
    (format "Client received {} bytes: {}"
      (LByteString.length bytes, decodeUtf8 bytes))

  case decode bytes of
    Just (Slack.Message chan_id (Slack.UserComment user_id) text _ _ _) ->
      io (enqueue (Message (Slack._getId chan_id) (Slack._getId user_id) text))

    _ -> pure ()
