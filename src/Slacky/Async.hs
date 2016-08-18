module Slacky.Async where

import Slacky.Prelude

import Control.Concurrent.Async   (Async)
import Control.Monad.Trans.Unlift

import qualified Control.Concurrent.Async as Async

async :: (MonadIO m, MonadBaseUnlift IO m) => m a -> m (Async a)
async m = do
  unlift <- askRunBase
  io (Async.async (unlift m))

withAsync :: (MonadIO m, MonadBaseUnlift IO m) => m a -> (Async a -> m b) -> m b
withAsync m k = do
  UnliftBase unlift <- askUnliftBase
  io (Async.withAsync (unlift m) (unlift . k))

cancel :: MonadIO m => Async a -> m ()
cancel = io . Async.cancel

waitCatch :: MonadIO m => Async a -> m (Either SomeException a)
waitCatch = io . Async.waitCatch

waitEither_ :: MonadIO m => Async a -> Async b -> m ()
waitEither_ x y = io (Async.waitEither_ x y)
