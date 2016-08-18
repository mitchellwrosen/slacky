{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Slacky.Prelude
  ( Data.ByteString.ByteString
  , LByteString
  , Data.Map.Map
  , Data.Text.Text
  , LText
  , Only(..)
  , encodeUtf8
  , decodeUtf8
  , format
  , pack
  , show
  , io
  , module Control.Exception.Safe
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Maybe
  , module Data.Monoid
  , module Prelude
  ) where

import Control.Exception.Safe hiding (Handler)
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString        (ByteString)
import Data.Map               (Map)
import Data.Maybe
import Data.Monoid
import Data.Text              (Text)
import Data.Text.Format       (Only(..), format)
import Prelude                hiding (log, show)

import qualified Data.ByteString.Lazy
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Prelude

type LByteString = Data.ByteString.Lazy.ByteString
type LText       = LText.Text

class Pack a where pack :: String -> a
instance Pack String where pack = id
instance Pack Text   where pack = Text.pack
instance Pack LText  where pack = LText.pack

class EncodeUtf8 a b | a -> b where encodeUtf8 :: a -> b
instance EncodeUtf8 Text  ByteString  where encodeUtf8 = Text.encodeUtf8
instance EncodeUtf8 LText LByteString where encodeUtf8 = LText.encodeUtf8

class DecodeUtf8 a b | a -> b where decodeUtf8 :: a -> b
instance DecodeUtf8 ByteString  Text  where decodeUtf8 = Text.decodeUtf8
instance DecodeUtf8 LByteString LText where decodeUtf8 = LText.decodeUtf8

show :: (Show a, Pack b) => a -> b
show = pack . Prelude.show

io :: MonadIO m => IO a -> m a
io = liftIO
