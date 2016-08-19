-- | Simple lens functionality. Get out of here!
module Control.Lens
  ( set
  ) where

-- https://www.stackage.org/lts-6.11/package/base
import Data.Functor.Identity

set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set f a b = runIdentity (f (\_ -> Identity a) b)
