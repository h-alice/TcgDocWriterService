{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Lib

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Exception            (try, SomeException)
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq as W
import VdbClient
import LmClient
import qualified Data.Aeson as A
import Control.Lens ((&), (.~), (^.))
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)



secondsToMicro :: Int -> Int
secondsToMicro s = s * 1000000

main :: IO ()
main = do
    putStrLn "Starting VDB Client..."
    let opts =   W.defaults & W.header "Content-Type" .~ ["application/json"]
                            & W.manager .~ Left (defaultManagerSettings { 
                                    managerResponseTimeout = responseTimeoutMicro $ secondsToMicro 300 } ) -- 5 minutes timeout
    let chatParams = LmParameters
            { temp = 0.7
            , topP = 0.9
            , frequencyPenalty = 0.0
            , presencePenalty = 0.0
            }
    let chatRecord = ChatRecord
            { crRole = "user"
            , crContent = "What is Haskell? Can you explain with a short rap?"
            }
    let chatRequest = mkChatRequest [chatRecord] "gpt-3.5-turbo" defaultLmParam
    response <- try (W.postWith opts "http://127.0.0.1:8089/v1/chat/completions" (A.toJSON chatRequest)) :: IO (Either SomeException (W.Response BL.ByteString))
    case response of
        Left err -> putStrLn $ "Error: " ++ show err
        Right res -> do
            let status = res ^. W.responseStatus
            let body =   res ^. W.responseBody
            putStrLn $ "Status: " ++ show status
            case A.eitherDecode body :: Either String ChatResponse of
                Left errMsg -> putStrLn $ "Failed to decode response: " ++ errMsg
                Right chatResponse -> do
                    putStrLn $ "Chat Response: " ++ show chatResponse
    putStrLn "Test Done!"
