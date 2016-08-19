module Main where

import Control.Lens
import Slacky.Prelude
import Slack.Types.RTM.Start (RTMStart)
import Slack.API.RTM.Start   (rtmStart)

-- https://www.stackage.org/lts-6.11/package/base
import Control.Exception
import Control.Monad
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
  -- This requires the LambdaCase language extension (see package.yaml).
  token <-
    getEnv' "SLACK_API_TOKEN" >>= \case
      Nothing -> do
        hPutStrLn stderr "Missing required env variable SLACK_API_TOKEN"
        exitWith (ExitFailure 1)
      Just x -> pure x

  response <-
    catch
      (rtmStart (pack token))
      (\e -> do
        hPutStrLn stderr (displayException (e :: SomeException))
        exitWith (ExitFailure 1))

  let code = responseStatusCode response
  when (code /= 200) $ do
    hPrint stderr code
    exitWith (ExitFailure 1)

  let body = responseBody response
  case decode body of
    Nothing -> do
      hPutStrLn stderr "Could not decode response body:"
      hPrint stderr body
      exitWith (ExitFailure 1)
    Just val -> print (val :: RTMStart)

-- | Like 'getEnv' from System.Environment, but instead of throwing a
-- synchronous exception when the environment variable is not found, return
-- Nothing.
getEnv' :: String -> IO (Maybe String)
getEnv' name =
  catchIOError
    (Just <$> getEnv name)
    (\e ->
      if isDoesNotExistError e
        then pure Nothing
        else ioError e)

responseStatusCode :: Wreq.Response LByteString -> Int
responseStatusCode = view (Wreq.responseStatus . Wreq.statusCode)

responseBody :: Wreq.Response LByteString -> LByteString
responseBody = view Wreq.responseBody
