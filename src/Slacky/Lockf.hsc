module Slacky.Lockf
  ( lockf
  ) where

import Slacky.Prelude

import Foreign.C
import System.Posix.Types (Fd(..))

#include "unistd.h"

foreign import ccall "lockf"
  c_lockf :: CInt -> CInt -> CInt -> IO CInt

lockf :: Fd -> IO Bool
lockf (Fd fd) = (== 0) <$> c_lockf fd (#const F_TLOCK) 0
