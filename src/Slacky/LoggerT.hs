{-# LANGUAGE UndecidableInstances #-}

module Slacky.LoggerT
  ( MonadLogger(..)
  , Severity(..)
  , LoggerT
  , logStdout
  , logStderr
  , logHandle
  , disableLogging
  ) where

import Slacky.Prelude

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import System.IO

import qualified Data.Text.Lazy.IO as LText

data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

class MonadLogger m where
  log :: Severity -> LText -> m ()

newtype LoggerT m a
  = LoggerT { runLoggerT :: ReaderT (Severity -> LText -> m ()) m a }
  deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask,
            MonadIO)

instance MonadTrans LoggerT where
  lift m = LoggerT (lift m)

instance MonadBase b m => MonadBase b (LoggerT m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (LoggerT m) where
  type StM (LoggerT m) a = StM m a

  -- Hold me...
  liftBaseWith
    :: forall a. ((forall x. LoggerT m x -> b (StM m x)) -> b a) -> LoggerT m a
  liftBaseWith k = LoggerT (ReaderT go)
   where
    go :: (Severity -> LText -> m ()) -> m a
    go lg =
      liftBaseWith
        (\runInBase -> k (\m -> runInBase (runReaderT (runLoggerT m) lg)))

  restoreM :: StM m a -> LoggerT m a
  restoreM x = lift (restoreM x)

instance MonadLogger (LoggerT m) where
  log severity msg = LoggerT (ReaderT (\f -> f severity msg))


logStdout :: MonadIO m => Severity -> LoggerT m a -> m a
logStdout = logHandle stdout

logStderr :: MonadIO m => Severity -> LoggerT m a -> m a
logStderr = logHandle stderr

logHandle :: MonadIO m => Handle -> Severity -> LoggerT m a -> m a
logHandle h threshold m =
  runReaderT (runLoggerT m)
    (\severity msg ->
      when (severity >= threshold)
        (io (LText.hPutStrLn h
          (format "[{}]: {}" (show severity :: LText, msg)))))

disableLogging :: Applicative m => LoggerT m a -> m a
disableLogging m = runReaderT (runLoggerT m) (\_ _ -> pure ())
