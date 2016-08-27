module Slacky.Monad
  ( Slacky
  , unliftIO
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
import System.IO

-- https://www.stackage.org/lts-6.11/package/transformers
import Control.Monad.IO.Class (MonadIO, liftIO)

-- https://www.stackage.org/lts-6.11/package/text
import qualified Data.Text.Lazy.IO as LText

-- | The severity of the message being logged.
data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

type LogFunction = Severity -> LText -> IO ()

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

-- | In the 'Slacky' monad, return a function that can unlift other 'Slacky'
-- computations to IO.
unliftIO :: Slacky (Slacky a -> IO a)
unliftIO = implementMe

-- | Run a 'Slacky' computation, logging all messages at or above the given
-- 'Severity' to stdout.
runSlackyStdout :: Severity -> Slacky a -> IO a
runSlackyStdout = implementMe

-- | Run a 'Slacky' computation, logging all messages at or above the given
-- 'Severity' to stderr.
runSlackyStderr :: Severity -> Slacky a -> IO a
runSlackyStderr = implementMe

-- | Log a debug message.
logDebug :: LText -> Slacky ()
logDebug = implementMe

-- | Log an info message.
logInfo :: LText -> Slacky ()
logInfo = implementMe

-- | Log a warning message.
logWarning :: LText -> Slacky ()
logWarning = implementMe

-- | Log an error message.
logError :: LText -> Slacky ()
logError = implementMe
