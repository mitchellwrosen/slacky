module Slacky.Backoff where

import Slacky.Async
import Slacky.Prelude

import Control.Concurrent
import Control.Monad.Trans.Unlift
import Data.IORef

type Threshold = Int
type Factor    = Int
type Seconds   = Int

million :: Int
million = 1000000

-- | @backoff threshold factor seconds action@ runs @action@ over and over, like
-- 'forever'. However, if it throws a synchronous exception, it's re-run after
-- @seconds@ seconds, then @factor * seconds@ seconds, and so on. But, if
-- @threshold@ seconds pass before the action throws a synchronous exception,
-- it's considered to have succeeded, and the time to wait is reset to
-- @seconds@.
backoff
  :: forall m a b.
     (MonadCatch m, MonadIO m, MonadBaseUnlift IO m)
  => Threshold -> Factor -> Seconds -> m a -> m b
backoff threshold fact s0 m = do
  sref <- io (newIORef s0)

  let reset :: m ()
      reset = io $ do
        threadDelay (threshold * million)
        writeIORef sref s0

      loop :: m b
      loop =
        withAsync reset (\_ -> tryAny m) >>= \case
          Left _ -> do
            s <- io (readIORef sref)
            io (threadDelay (s * million))
            io (writeIORef sref (s * fact))
            loop
          Right _ -> loop

  loop
