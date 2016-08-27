module Slacky.Monad
  ( Slacky
  , runSlackyStdout
  , runSlackyStderr
  , Severity(..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  ) where

import Slacky.Prelude

-- https://www.stackage.org/lts-6.11/package/base
import Control.Monad (ap)

-- https://www.stackage.org/lts-6.11/package/transformers
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | The severity of the message being logged.
data Severity
  = Debug
  | Info
  | Warning
  | Error

type LogFunction = Severity -> Text -> IO ()

newtype Slacky a
  = Slacky { runSlacky :: LogFunction -> IO a }

instance Functor Slacky where
  fmap :: (a -> b) -> Slacky a -> Slacky b
  fmap = implementMe

instance Applicative Slacky where
  pure :: a -> Slacky a
  pure = return

  (<*>) :: Slacky (a -> b) -> Slacky a -> Slacky b
  (<*>) = ap

instance Monad Slacky where
  return :: a -> Slacky a
  return = implementMe

  (>>=) :: Slacky a -> (a -> Slacky b) -> Slacky b
  (>>=) = implementMe

instance MonadIO Slacky where
  liftIO :: IO a -> Slacky a
  liftIO = implementMe

-- | Run a 'Slacky' computation, logging all messages at or above the given
-- 'Severity' to stdout.
runSlackyStdout :: Severity -> Slacky a -> IO a
runSlackyStdout = implementMe

-- | Run a 'Slacky' computation, logging all messages at or above the given
-- 'Severity' to stderr.
runSlackyStderr :: Severity -> Slacky a -> IO a
runSlackyStderr = implementMe

-- | Log a debug message.
logDebug :: Text -> Slacky ()
logDebug = implementMe

-- | Log an info message.
logInfo :: Text -> Slacky ()
logInfo = implementMe

-- | Log a warning message.
logWarning :: Text -> Slacky ()
logWarning = implementMe

-- | Log an error message.
logError :: Text -> Slacky ()
logError = implementMe
