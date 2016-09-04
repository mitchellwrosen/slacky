module Slacky.Async where

import Slacky.Prelude

import Control.Concurrent.Async   (Async)
import Control.Monad.Trans.Unlift

import qualified Control.Concurrent.Async as Async

withAsync :: (MonadIO m, MonadBaseUnlift IO m) => m a -> (Async a -> m b) -> m b
withAsync m k = do
  UnliftBase unlift <- askUnliftBase
  io (Async.withAsync (unlift m) (unlift . k))

race_ :: (MonadIO m, MonadBaseUnlift IO m) => m a -> m b -> m ()
race_ x y = do
  UnliftBase unlift <- askUnliftBase
  io (Async.race_ (unlift x) (unlift y))
