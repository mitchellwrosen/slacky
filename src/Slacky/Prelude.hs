module Slacky.Prelude
  ( Data.Text.Text
  , LByteString
  , implementMe
  ) where

-- https://www.stackage.org/lts-6.11/package/bytestring
import qualified Data.ByteString.Lazy

-- https://www.stackage.org/lts-6.11/package/text
import qualified Data.Text

type LByteString = Data.ByteString.Lazy.ByteString

implementMe :: a
implementMe = error "Implement me!"
