-- | A random assortment of IO operations lifted to the Slacky monad.
module Slacky.Lifted where

import Slacky.Monad
import Slacky.Prelude

-- https://www.stackage.org/lts-6.11/package/base
import Control.Exception

liftedCatch :: Exception e => Slacky a -> (e -> Slacky a) -> Slacky a
liftedCatch = implementMe
