module Main where

import Slack.API.RTM.Start
import Slacky.Client
import Slacky.Lockf
import Slacky.Globals
import Slacky.Prelude
import Slacky.Server

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Text                (pack)
import System.Directory
import System.Exit
import System.IO
import System.Posix.Env         (getEnv)
import System.Posix.Files       (accessModes)
import System.Posix.IO
import System.Posix.Process
import Wuss                     (runSecureClient)

import qualified Network.WebSockets as WebSockets

main :: IO ()
main = do
  token <- initSlacky

  RtmInfo{..} <- rtmStart token

  -- TODO: add option to daemonize

  pid <- getProcessID
  writeFile globalPidfile (show pid)

  runSecureClient rtmHost 443 rtmPath $ \conn -> do
    msg_queue <- newTQueueIO

    serverState <-
      newServerState
        (atomically (tryReadTQueue msg_queue)) (WebSockets.sendTextData conn)
          rtmNames

    let server :: IO ()
        server = runDomainSocket globalSockfile (slackyServer serverState)

        client :: IO ()
        client =
          slackyClient
            (clientState (atomically . writeTQueue msg_queue)
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
-- Returns the SLACK_API_TOKEN.
initSlacky :: IO ApiToken
initSlacky = do
  home <- getHomeDirectory
  token <- requireEnv "SLACK_API_TOKEN"

  let slackyDir = home ++ ('/':globalSlackyDir)

  createDirectoryIfMissing True slackyDir
  setCurrentDirectory slackyDir

  bracket
    (openFd globalPidfile ReadWrite (Just accessModes) defaultFileFlags)
    closeFd
    (\pidfile -> do
      locked <- lockf pidfile
      unless locked $ do
        pid <- hGetContents =<< fdToHandle pidfile
        hPutStrLn stderr ("Already running: pid " ++ show pid)
        exitWith (ExitFailure 1))

  pure (pack token)

requireEnv :: String -> IO String
requireEnv x =
  getEnv x >>= \case
    Nothing -> do
      hPutStrLn stderr ("Missing required environment variable: " ++ x)
      exitWith (ExitFailure 1)
    Just y -> pure y


-- Note [Unrolled race]
--
-- This is *almost* the 'race_' combinator, but 'cancel' is not a sufficient
-- cleanup action for the server, because it has cleanup to perform as well
-- (namely, deleting the pidfile).
--
-- With normal 'withAsync', the cancel's 'throwTo' returns when the
-- ThreadKilled exception is delivered, and then the main thread quits, not
-- necessarily leaving enough time for all of the server's exception handlers
-- to run.
--
-- So, in *our* cleanup action, first send ThreadKilled ('cancel'), then wait
-- on the result var, trusting the server to clean up in a timely manner.
--
-- If not (unlikey? impossible?), a second Ctrl+C will work, and the pidfile
-- might be left on disk. Oh well.
