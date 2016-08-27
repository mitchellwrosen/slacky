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
import Control.Monad (ap, when)
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
  fmap f m = Slacky (fmap f . runSlacky m)

instance Applicative Slacky where
  pure :: a -> Slacky a
  pure = return

  (<*>) :: Slacky (a -> b) -> Slacky a -> Slacky b
  (<*>) = ap

instance Monad Slacky where
  return :: a -> Slacky a
  return x = Slacky (\_ -> return x)

  (>>=) :: Slacky a -> (a -> Slacky b) -> Slacky b
  m >>= f = Slacky (\g -> do
    a <- runSlacky m g
    runSlacky (f a) g)

instance MonadIO Slacky where
  liftIO :: IO a -> Slacky a
  liftIO m = Slacky (\_ -> m)

-- | In the 'Slacky' monad, return a function that can unlift other 'Slacky'
-- computations to IO.
unliftIO :: Slacky (Slacky a -> IO a)
unliftIO = Slacky (\g -> pure (\m -> runSlacky m g))

-- | Run a 'Slacky' computation, logging all messages at or above the given
-- 'Severity' to stdout.
runSlackyStdout :: Severity -> Slacky a -> IO a
runSlackyStdout = runSlackyHandle stdout

-- | Run a 'Slacky' computation, logging all messages at or above the given
-- 'Severity' to stderr.
runSlackyStderr :: Severity -> Slacky a -> IO a
runSlackyStderr = runSlackyHandle stderr

runSlackyHandle :: Handle -> Severity -> Slacky a -> IO a
runSlackyHandle h threshold m =
  runSlacky m (\severity msg ->
    when (severity >= threshold)
      (LText.hPutStrLn h (format "[{}]: {}" (show severity, msg))))

-- | Log a debug message.
logDebug :: LText -> Slacky ()
logDebug msg = Slacky (\g -> g Debug msg)

-- | Log an info message.
logInfo :: LText -> Slacky ()
logInfo msg = Slacky (\g -> g Info msg)

-- | Log a warning message.
logWarning :: LText -> Slacky ()
logWarning msg = Slacky (\g -> g Warning msg)

-- | Log an error message.
logError :: LText -> Slacky ()
logError msg = Slacky (\g -> g Error msg)
