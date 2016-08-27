module Slacky.Prelude
  ( Text.Text
  , LText
  , LByteString
  , implementMe
  , pack
  , unpack
  , io
  , Only(..)
  , format
  ) where

-- https://www.stackage.org/lts-6.11/package/transformers
import Control.Monad.IO.Class (MonadIO, liftIO)

-- https://www.stackage.org/lts-6.11/package/bytestring
import qualified Data.ByteString.Lazy    as LByteString

-- https://www.stackage.org/lts-6.11/package/text
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText

-- https://www.stackage.org/lts-6.11/package/text-format-0.3.1.1
import Data.Text.Format (Only(..), format)

type LByteString = LByteString.ByteString

type LText = LText.Text

implementMe :: a
implementMe = error "Implement me!"

class Pack a where
  pack :: String -> a

instance Pack Text.Text where
  pack = Text.pack

instance Pack LText.Text where
  pack = LText.pack

class Unpack a where
  unpack :: a -> String

instance Unpack Text.Text where
  unpack = Text.unpack

io :: MonadIO m => IO a -> m a
io = liftIO
