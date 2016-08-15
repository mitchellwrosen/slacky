module Slacky.Daemon
  ( daemon
  ) where

import Slacky.Prelude

import Control.Concurrent   (myThreadId)
import Control.Exception    (AsyncException(UserInterrupt))
import System.Exit
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals

daemon :: IO () -> IO ()
daemon act = do
  n <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
  mapM_ (dupTo n) [stdInput, stdOutput, stdError]

  _ <- forkProcess $ do
    _ <- createSession
    _ <- forkProcess $ do
      tid <- myThreadId

      let handler :: Handler
          handler = CatchOnce (throwTo tid UserInterrupt)

      _ <- installHandler sigINT  handler Nothing
      _ <- installHandler sigQUIT handler Nothing
      _ <- installHandler sigTERM handler Nothing

      act
    exitImmediately ExitSuccess
  exitImmediately ExitSuccess
