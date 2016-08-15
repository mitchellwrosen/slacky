module Main where

import Slack.API.RTM.Start
import Slacky.Client
import Slacky.Globals
import Slacky.Lockf
import Slacky.Logger
import Slacky.Prelude
import Slacky.Server

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Aeson               (decode)
import Data.Tuple               (swap)
import System.Directory
import System.Exit
import System.IO
import System.Posix.Env         (getEnv)
import System.Posix.Files       (accessModes)
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types       (ProcessID)
import Wuss                     (runSecureClient)

import qualified Data.Map           as Map
import qualified Network.WebSockets as WebSockets

main :: IO ()
main = initSlacky (logStdout Debug) main'

main' :: Logger -> (ProcessID -> IO ()) -> ApiToken -> IO ()
main' log writePid token = do
  log Info (format "GET {}" (Only rtmStartUrl))

  bytes <- rtmStart token
  log Debug (decodeUtf8 bytes)

  RtmInfo{..} <-
    case decode bytes of
      Nothing -> do
        -- Just be dumb and assume the HTTP response is UTF-8 encoded, since my
        -- dumb logging framework can only log Text.
        log Error ("Could not decode response: " <> decodeUtf8 bytes)
        exitWith (ExitFailure 1)
      Just info -> pure info

  -- TODO: add option to daemonize

  getProcessID >>= writePid

  log Info (format "Connecting to wss://{}:443{}" (rtmHost, rtmPath))

  runSecureClient rtmHost 443 rtmPath $ \conn -> do
    msg_queue <- newTQueueIO

    serverState <-
      newServerState
        log
        (atomically (emptyTQueue msg_queue))
        (WebSockets.sendTextData conn)
        (rtmUsers <> rtmChannels <> rtmGroups <> rtmIMs)
        (invertMap (rtmChannels <> rtmGroups <> rtmIMs))

    let server :: IO ()
        server = runDomainSocket log globalSockfile (slackyServer serverState)

        client :: IO ()
        client =
          slackyClient
            (clientState log (atomically . writeTQueue msg_queue)
              (WebSockets.receiveData conn))

    -- Note [Unrolled race]
    bracket (async server) (\a -> cancel a >> waitCatch a) (\a1 ->
      withAsync client $ \a2 ->
        waitEither_ a1 a2)

-- | Does the following:
--
--   - ensure HOME and SLACK_API_TOKEN env variables exist
--   - cd to ~/.config/slacky
--   - attempt to acquire lock on pidfile
--
-- If successfull, passes the "write pid to pidfile" action and the Slack API
-- token to the provided continuation.
initSlacky
  :: Logger -> (Logger -> (ProcessID -> IO ()) -> ApiToken -> IO ()) -> IO ()
initSlacky log act = do
  home <- getHomeDirectory

  token <-
    getEnv "SLACK_API_TOKEN" >>= \case
      Nothing -> do
        log Error ("Missing required environment variable: SLACK_API_TOKEN")
        exitWith (ExitFailure 1)
      Just x -> pure x

  let slackyDir = home ++ ('/':globalSlackyDir)

  createDirectoryIfMissing True slackyDir
  setCurrentDirectory slackyDir

  bracket
    (openFd globalPidfile ReadWrite (Just accessModes) defaultFileFlags)
    closeFd
    (\fd -> do
      log Debug "Acquiring lock on pidfile"
      lockf fd >>= \case
        True  -> act log (void . fdWrite fd . show) (pack token)
        False -> do
          pid <- hGetContents =<< fdToHandle fd
          log Error ("Already running: pid " <> show pid)
          exitWith (ExitFailure 1))

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

-- Note [Unrolled race]
--
-- This is *almost* the 'race_' combinator, but 'cancel' is not a sufficient
-- cleanup action for the server, because it has cleanup to perform as well
-- (namely, deleting the sockfile).
--
-- With normal 'withAsync', the cancel's 'throwTo' returns when the
-- ThreadKilled exception is delivered, and then the main thread quits, not
-- necessarily leaving enough time for all of the server's exception handlers
-- to run.
--
-- So, in *our* cleanup action, first send ThreadKilled ('cancel'), then wait
-- on the result var, trusting the server to clean up in a timely manner.
--
-- If not (unlikely? impossible?), a second Ctrl+C will work, and the sockfile
-- might be left on disk. Oh well.
