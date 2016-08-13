module Slacky.Prelude
  ( Data.ByteString.ByteString
  , LByteString
  , Data.Map.Map
  , Data.Text.Text
  , LText
  , module Control.Exception.Safe
  , module Control.Monad
  , module Data.Maybe
  , module Data.Monoid
  ) where

import Control.Exception.Safe hiding (Handler)
import Control.Monad
import Data.Maybe
import Data.Monoid

import qualified Data.Map
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Lazy

type LByteString = Data.ByteString.Lazy.ByteString
type LText = Data.Text.Lazy.Text
