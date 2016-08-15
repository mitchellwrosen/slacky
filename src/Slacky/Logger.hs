module Slacky.Logger
  ( Logger
  , Severity(..)
  , logStdout
  , logStderr
  , disableLogging
  ) where

import Slacky.Prelude

import System.IO

import qualified Data.Text.Lazy.IO as LText

data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

type Logger = Severity -> LText -> IO ()

logStdout :: Severity -> Logger
logStdout = logHandle stdout

logStderr :: Severity -> Logger
logStderr = logHandle stderr

logHandle :: Handle -> Severity -> Logger
logHandle h threshold severity msg =
  when (severity >= threshold)
    (LText.hPutStrLn h (format "[{}]: {}" (show severity :: LText, msg)))

disableLogging :: Logger
disableLogging _ _ = pure ()
