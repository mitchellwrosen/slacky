module Main where

import Control.Lens
import Slacky.Lifted
import Slacky.Prelude
import Slacky.Monad
import Slack.Types.RTM.Start (RTMStart)
import Slack.API.RTM.Start   (rtmStart)

-- https://www.stackage.org/lts-6.11/package/base
import Control.Exception
import Control.Monad
import System.Environment
import System.Exit
import System.IO.Error

-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson

-- https://www.stackage.org/lts-6.11/package/text-format-0.3.1.1
import Data.Text.Format (Shown(..))

-- https://www.stackage.org/lts-6.11/package/websockets
import qualified Network.WebSockets as WebSockets

-- https://www.stackage.org/lts-6.11/package/wreq
import qualified Network.Wreq as Wreq

-- Run 'slackyMain' with the desired logging method (to stdout or to stderr) and
-- the desired threshold (Debug).
main :: IO ()
main = runSlackyStderr Debug slackyMain

-- Port the 'oldMain' code to the 'Slacky' monad by peppering all function calls
-- in the IO monad with 'io' (exported by Slacky.Prelude). Then, replace the
-- existing explicit writes to stderr to use the logging API. Then, add some
-- more log messages.
slackyMain :: Slacky ()
slackyMain = do
  token <- do
    io (getEnv' "SLACK_API_TOKEN") >>= \case
      Nothing -> do
        logError "Missing required env variable SLACK_API_TOKEN"
        io (exitWith (ExitFailure 1))
      Just x -> pure x

  response <-
    liftedCatch
      (io (rtmStart (pack token)))
      (\e -> do
        logError (pack (displayException (e :: SomeException)))
        io (exitWith (ExitFailure 1)))

  let code = responseStatusCode response
  when (code /= 200) $ do
    logError (pack (show code))
    io (exitWith (ExitFailure 1))

  let body = responseBody response

  -- I re-structured this a bit from last time, to help with indentation creep.
  -- The record wildcard pulls things like rtmStartHost, rtmStartPath, etc. into
  -- top-level scope. That is,
  --
  --   rtmStartHost :: String
  --
  -- in this context, even though normally it's a projection function with type
  --
  --   rtmStartHost :: RTMStart -> String
  --
  RTMStart{..} <-
    case decode body of
      Nothing -> do
        logError
          (format "Could not decode response body: {}" (Only (Shown body)))
        io (exitWith (ExitFailure 1))
      Just val -> pure val

  -- Initialize a connection context
  context <- implementMe

  -- Connect to the Slack host on port 443 over TLS. Remember to clean up the
  -- collection in an exception-safe way! (You'll need to implement
  -- 'liftedBracket' for this. Head over to src/Slacky/Lifted.hs).
  implementMe

  -- Inside the above callback, make a WebSockets stream (again in an
  -- exception-safe way!)
  implementMe

  -- Inside the callback's callback, run our websocket client! I've stubbed it
  -- out as 'client' below.
  implementMe

client :: WebSockets.Connection -> Slacky ()
client conn = do
  -- Loop forever, printing out (as Text) every message received.
  implementMe

-- | Like 'getEnv' from System.Environment, but instead of throwing a
-- synchronous exception when the environment variable is not found, return
-- Nothing.
getEnv' :: String -> IO (Maybe String)
getEnv' name =
  catchIOError
    (Just <$> getEnv name)
    (\e ->
      if isDoesNotExistError e
        then pure Nothing
        else ioError e)

responseStatusCode :: Wreq.Response LByteString -> Int
responseStatusCode = view (Wreq.responseStatus . Wreq.statusCode)

responseBody :: Wreq.Response LByteString -> LByteString
responseBody = view Wreq.responseBody
