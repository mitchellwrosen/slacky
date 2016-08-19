-- | Simple lens functionality. Get out of here!
module Control.Lens
  ( set
  , view
  ) where

-- https://www.stackage.org/lts-6.11/package/base
import Control.Applicative
import Data.Functor.Identity

set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set f a b = runIdentity (f (\_ -> Identity a) b)

view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
view f b = getConst (f Const b)
