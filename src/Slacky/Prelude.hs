module Slacky.Prelude
  ( Data.Text.Text
  , LByteString
  , implementMe
  ) where

import qualified Data.ByteString.Lazy
import qualified Data.Text

type LByteString = Data.ByteString.Lazy.ByteString

implementMe :: a
implementMe = error "Implement me!"
