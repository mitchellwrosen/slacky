-- | A random assortment of IO operations lifted to the Slacky monad.
module Slacky.Lifted where

import Slacky.Monad
import Slacky.Prelude

-- https://www.stackage.org/lts-6.11/package/base
import Control.Exception

-- https://www.stackage.org/lts-6.11/package/websockets
import qualified Network.WebSockets        as WebSockets
import qualified Network.WebSockets.Stream as WebSockets

liftedCatch :: Exception e => Slacky a -> (e -> Slacky a) -> Slacky a
liftedCatch m k = do
  unlift <- unliftIO
  io (catch (unlift m) (\e -> unlift (k e)))

-- | Like 'bracket', but in the Slacky monad.
liftedBracket :: Slacky a -> (a -> Slacky b) -> (a -> Slacky c) -> Slacky c
liftedBracket open close inner = implementMe

-- | Like 'runClientWithStream', but in the Slacky monad.
liftedRunClientWithStream
  :: WebSockets.Stream -> String -> String -> WebSockets.ConnectionOptions
  -> WebSockets.Headers -> (WebSockets.Connection -> Slacky a) -> Slacky a
liftedRunClientWithStream = implementMe
