module Main where

import Slack.API.RTM.Start
import Slacky.Async
import Slacky.Backoff
import Slacky.Client
import Slacky.Globals
import Slacky.Lockf
import Slacky.LoggerT
import Slacky.Prelude
import Slacky.Server

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Unlift
import Data.Aeson                 (decode)
import Data.Tuple                 (swap)
import Network.Socket             (HostName, PortNumber)
import System.Directory
import System.Exit
import System.IO
import System.Posix.Env           (getEnv)
import System.Posix.Files         (accessModes)
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types         (ProcessID)
import Wuss                       (runSecureClient)

import qualified Data.Map           as Map
import qualified Network.WebSockets as WebSockets

main :: IO ()
main = logStdout Debug (initSlacky main')

main'
  :: (MonadLogger m, MonadMask m, MonadIO m, MonadBaseUnlift IO m)
  => (ProcessID -> IO ()) -> ApiToken -> m ()
main' writePid token = do
  -- TODO: add option to daemonize

  io (getProcessID >>= writePid)

  backoff 30 2 2 $ do
    log Info (format "GET {}" (Only rtmStartUrl))

    bytes <- rtmStart token
    log Debug (decodeUtf8 bytes)

    info@RtmInfo{..} <-
      case decode bytes of
        Nothing -> do
          -- Just be dumb and assume the HTTP response is UTF-8 encoded, since
          -- my dumb logging framework can only log Text.
          log Error ("Could not decode response: " <> decodeUtf8 bytes)
          io (exitWith (ExitFailure 1))
        Just x -> pure x

    log Info (format "Connecting to wss://{}:443{}" (rtmHost, rtmPath))
    liftedRunSecureClient rtmHost 443 rtmPath (wsClient info)

liftedRunSecureClient
  :: (MonadIO m, MonadBaseUnlift IO m)
  => HostName -> PortNumber -> String -> (WebSockets.Connection -> m a) -> m a
liftedRunSecureClient host port path app = do
  unlift <- askRunBase
  io (runSecureClient host port path (unlift . app))

wsClient
  :: forall m.
     (MonadLogger m, MonadMask m, MonadIO m, MonadBaseUnlift IO m)
  => RtmInfo -> WebSockets.Connection -> m ()
wsClient RtmInfo{..} conn = do
  msg_queue <- io (newMVar [])

  serverState <-
    io (newServerState
      (swapMVar msg_queue [])
      (WebSockets.sendTextData conn)
      (rtmUsers <> rtmChannels <> rtmGroups <> rtmIMs)
      (invertMap (rtmChannels <> rtmGroups <> rtmIMs)))

  let server :: m ()
      server = runDomainSocket globalSockfile (slackyServer serverState)

      client :: m ()
      client =
        slackyClient
          (clientState (\m -> modifyMVarMasked_ msg_queue (\ms -> pure (m:ms)))
            (WebSockets.receiveData conn))

  race_ server client

-- | Does the following:
--
--   - ensure HOME and SLACK_API_TOKEN env variables exist
--   - cd to ~/.config/slacky
--   - attempt to acquire lock on pidfile
--
-- If successfull, passes the "write pid to pidfile" action and the Slack API
-- token to the provided continuation.
initSlacky
  :: (MonadLogger m, MonadMask m, MonadIO m)
  => ((ProcessID -> IO ()) -> ApiToken -> m ()) -> m ()
initSlacky act = do
  home <- io getHomeDirectory

  token <-
    io (getEnv "SLACK_API_TOKEN") >>= \case
      Nothing -> do
        log Error ("Missing required environment variable: SLACK_API_TOKEN")
        io (exitWith (ExitFailure 1))
      Just x -> pure x

  let slackyDir = home ++ ('/':globalSlackyDir)

  io (createDirectoryIfMissing True slackyDir)
  io (setCurrentDirectory slackyDir)

  bracket
    (io (openFd globalPidfile ReadWrite (Just accessModes) defaultFileFlags))
    (io . closeFd)
    (\fd -> do
      log Debug "Acquiring lock on pidfile"
      io (lockf fd) >>= \case
        True  -> act (void . fdWrite fd . show) (pack token)
        False -> do
          pid <- io (hGetContents =<< fdToHandle fd)
          log Error ("Already running: pid " <> show pid)
          io (exitWith (ExitFailure 1)))

-- | Empty a TQueue in a single giant transaction, returning elements in
-- reverse order. Careful - this will retry like crazy if the queue is hot.
emptyTQueue :: TQueue a -> STM [a]
emptyTQueue q = go []
 where
  go xs =
    tryReadTQueue q >>= \case
      Nothing -> pure xs
      Just x  -> go (x:xs)

invertMap :: Ord v => Map k v -> Map v k
invertMap = Map.fromList . map swap . Map.assocs
