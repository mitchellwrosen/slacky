{-# LANGUAGE CApiFFI #-}

module Slacky.Lockf
  ( lockf
  ) where

import Foreign.C
import System.Posix.Types (Fd(..))

foreign import capi
  "lockf"
  c_lockf :: CInt -> CInt -> CInt -> IO CInt

foreign import capi
  "unistd.h value F_TLOCK"
  fTLOCK :: CInt

lockf :: Fd -> IO Bool
lockf (Fd fd) = (== 0) <$> c_lockf fd fTLOCK 0
