module Main where

import Control.Lens
import Slacky.Prelude

-- https://www.stackage.org/lts-6.11/package/base
import System.Environment
import System.Exit
import System.IO
import System.IO.Error

-- https://www.stackage.org/lts-6.11/package/aeson
import Data.Aeson

-- https://www.stackage.org/lts-6.11/package/wreq
import qualified Network.Wreq as Wreq

main :: IO ()
main = do
  -- Check for environment variable "SLACK_API_TOKEN", and exit if it doesn't
  -- exist. Use the helper function getEnv', stubbed below.
  implementMe

  -- Pass the token to 'rtmStart' as a Text. You can convert from a String to a
  -- Text using 'pack', imported from Slacky.Prelude.
  --
  -- Extra credit: catch any IO exceptions thrown by 'rtmStart', and print some
  -- nice error message before ultimately exiting. To accomplish this, import
  -- the Control.Exception module from base.
  implementMe

  -- Check the status code of the response using the 'responseStatusCode'
  -- helper function implemented below. If it's not 200, print it and exit.
  implementMe

  -- Then, extract the reponse body using the 'responseBody' helper function
  -- implemented below, and decode it into an RTMStart using the Data.Aeson
  -- module. If this fails, print an error message and the un-parseable blob of
  -- data. If it succeeds, print the RTMStart structure.
  implementMe

-- | Like 'getEnv' from System.Environment, but instead of throwing a
-- synchronous exception when the environment variable is not found, return
-- Nothing.
--
-- Poke around the System.IO.Error module for some simple examples.
getEnv' :: String -> IO (Maybe String)
getEnv' = implementMe

responseStatusCode :: Wreq.Response LByteString -> Int
responseStatusCode = view (Wreq.responseStatus . Wreq.statusCode)

responseBody :: Wreq.Response LByteString -> LByteString
responseBody = view Wreq.responseBody
