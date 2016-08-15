module Slacky.Client
  ( ClientState
  , clientState
  , slackyClient
  ) where

import Slacky.Logger
import Slacky.Message
import Slacky.Prelude

import Data.Aeson

import qualified Data.ByteString.Lazy as LByteString
import qualified Web.Slack            as Slack

data ClientState = ClientState
  { log     :: Logger
  , enqueue :: Message -> IO ()
  , recv    :: IO LByteString
  }

clientState :: Logger -> (Message -> IO ()) -> IO LByteString -> ClientState
clientState = ClientState

slackyClient :: ClientState -> IO ()
slackyClient ClientState{..} = forever $ do
  bytes <- recv

  log Debug
    (format "Client received {} bytes" (Only (LByteString.length bytes)))

  case decode bytes of
    Just (Slack.Message chan_id (Slack.UserComment user_id) text _ _ _) ->
      enqueue (Message (Slack._getId chan_id) (Slack._getId user_id) text)

    _ ->
      -- Just assume UTF-8 encoding for printing, since pringing out a hex
      -- representation seems less than ideal.
      log Debug
        (format "Client ignoring message: {}" (Only (decodeUtf8 bytes)))
