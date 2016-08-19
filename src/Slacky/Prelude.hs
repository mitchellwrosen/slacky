module Slacky.Prelude
  ( Text.Text
  , LByteString
  , implementMe
  , pack
  , unpack
  ) where

-- https://www.stackage.org/lts-6.11/package/bytestring
import qualified Data.ByteString.Lazy    as LByteString

-- https://www.stackage.org/lts-6.11/package/text
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText

type LByteString = LByteString.ByteString

implementMe :: a
implementMe = error "Implement me!"

class Pack a where
  pack :: String -> a

instance Pack Text.Text where
  pack = Text.pack

class Unpack a where
  unpack :: a -> String

instance Unpack Text.Text where
  unpack = Text.unpack
