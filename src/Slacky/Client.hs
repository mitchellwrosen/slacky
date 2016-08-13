module Slacky.Client
  ( ClientState
  , clientState
  , slackyClient
  ) where

import Slacky.Message
import Slacky.Prelude

import Data.Aeson

import qualified Web.Slack as Slack

data ClientState = ClientState
  { enqueue :: Message -> IO ()
  , recv    :: IO LByteString
  }

clientState :: (Message -> IO ()) -> IO LByteString -> ClientState
clientState = ClientState

slackyClient :: ClientState -> IO ()
slackyClient ClientState{..} = forever $ do
  bytes <- recv
  case decode bytes of
    Just (Slack.Message chan_id (Slack.UserComment user_id) text _ _ _) ->
      enqueue (Message (Slack._getId chan_id) (Slack._getId user_id) text)
    _ -> pure ()
