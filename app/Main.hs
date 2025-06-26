{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Lib

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Exception            (try, SomeException)
import Data.List
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq as W
import VdbClient
import LmClient
import ApiCore
import qualified Data.Aeson as A
import Control.Lens ((&), (.~), (^.))
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)
import Config
import qualified Data.Text.IO as TIO
-- Web Server Core (WAI & Warp)
import Network.Wai ( Application, pathInfo, requestMethod, responseLBS, lazyRequestBody )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types ( status200, status400, status404, status405, status418, status500 )
import Network.HTTP.Types.Header ( hContentType )
import Control.Monad.IO.Class (liftIO) -- To lift IO actions within WAI Application monad
import System.IO (hPutStrLn, stderr, stdout) -- For printing errors/warnings
import Data.Either


-- Utils

eitherListVerifier :: [Either String a] -> Either String [a]
eitherListVerifier [] = Right []
eitherListVerifier (x:xs) = case x of
    Left err -> Left err
    Right val -> case eitherListVerifier xs of
        Left err -> Left err
        Right vals -> Right (val:vals)

chatRecordFromFeThread :: FrontEndThread -> [ChatRecord]
chatRecordFromFeThread FrontEndThread{ftMessages} =
    map (\FrontEndMessage{fmRole, fmMessage} -> ChatRecord
        { crRole = fmRole
        , crContent = fmMessage
        }) ftMessages

sysPromptInjector :: FrontEndThread -> Config -> FrontEndThread
sysPromptInjector FrontEndThread{ftMessages, fmVdbConfig, fmGeneratorConfig, ftThreadId} config =
    let systemMessage = FrontEndMessage
            { fmMessageId = "system"
            , fmRole = "system"
            , fmMessage = (gcSystem . cfgGenerator) config
            , fmRewriteFlag = False
            }
    in FrontEndThread
        { ftMessages = systemMessage : ftMessages
        , fmVdbConfig = fmVdbConfig
        , fmGeneratorConfig = fmGeneratorConfig
        , ftThreadId = ftThreadId
        }

-- Workers

rewriteWorker :: FrontEndMessage -> Config -> ApiVdbConfig -> IO (Either String FrontEndMessage)
rewriteWorker FrontEndMessage{fmMessageId, fmRole, fmMessage, fmRewriteFlag} config vdbConf = do
    if not fmRewriteFlag || (fmRole /= "user") then return $ Right FrontEndMessage{fmMessageId, fmRole, fmMessage, fmRewriteFlag} 
    else do
        let rwConfig = gcRewriter $ cfgGenerator config
        let promptTemplate = rwPromptTemplate rwConfig
        let docPlaceholder = rwDocPlaceholder rwConfig
        let userPlaceholder = rwUserPlaceholder rwConfig

        let request = RetrievalRequest
              { reqId = fmMessageId
              , reqCollection = avCollection vdbConf
              , reqQuery = fmMessage
              , reqQueryParams = RetrievalParameters
                  { paramTopK = avTopK vdbConf
                  , paramPoolSize = avPoolSize vdbConf
                  , paramAlpha = avAlpha vdbConf
                  }
              }

        -- Log the request details (optional)
        liftIO $ hPutStrLn stderr $ "[Rewriter] Requesting documents for query: " ++ T.unpack fmMessage

        docs <- retrievalDocument (T.unpack $ cfgVdbEndpoint config) request

        case docs of
            Left err -> do
                liftIO $ hPutStrLn stderr $ "[Rewriter] Error retrieving documents: " ++ err
                return $ Left $ "Error retrieving documents: " ++ err
            Right retrievedDocs -> do

                -- retrievedDocs is a list of Text documents
                let docText = T.intercalate "\n\n=====\n\n" (respDocuments retrievedDocs)

                liftIO $ TIO.hPutStrLn stderr $ "[Rewriter] Retrieved documents: " <> docText

                -- Replace placeholders in the prompt template
                let rewrittenQuery =  T.replace docPlaceholder docText $
                                      T.replace userPlaceholder fmMessage promptTemplate

                return $ Right FrontEndMessage{fmMessageId, fmRole, fmMessage = rewrittenQuery, fmRewriteFlag}

promptRewriter :: FrontEndThread -> Config -> IO (Either String FrontEndThread)
promptRewriter FrontEndThread{ftMessages, fmVdbConfig, fmGeneratorConfig, ftThreadId} config = do
    -- Log the start of the rewriting process
    liftIO $ hPutStrLn stderr $ "[Rewriter] Starting prompt rewriting for thread: " ++ T.unpack ftThreadId
    -- Process each message in the thread
    let rewriteTasks = map (\msg -> rewriteWorker msg config fmVdbConfig) ftMessages
    -- Execute all rewrite tasks
    results <- sequence rewriteTasks
    case eitherListVerifier results of
        Left err -> do
            liftIO $ hPutStrLn stderr $ "[Rewriter] Error occurred while rewriting prompts: " ++ err
            return $ Left $ "Error occurred while rewriting prompts: " ++ err
        Right rewrittenMessages -> do
            liftIO $ hPutStrLn stderr $ "[Rewriter] Successfully rewritten prompts for thread: " ++ T.unpack ftThreadId
            return $ Right FrontEndThread{ftMessages = rewrittenMessages, fmVdbConfig, fmGeneratorConfig, ftThreadId}

respGenerator :: FrontEndThread -> Config -> IO (Either String FrontEndMessage)
respGenerator feThread config = do
    -- Log the start of the response generation process
    liftIO $ hPutStrLn stderr $ "[Generator] Starting response generation for thread: " ++ T.unpack (ftThreadId feThread)

    let generatorParams = LmParameters
            { temp = agTemperature $ fmGeneratorConfig feThread
            , topP = agTopP $ fmGeneratorConfig feThread
            , frequencyPenalty = agFrequencyPenalty $ fmGeneratorConfig feThread
            , presencePenalty = agPresencePenalty $ fmGeneratorConfig feThread
            }
    -- Extract the last user message
    let chatRecords = chatRecordFromFeThread feThread
    case chatRecords of
        [] -> do
            liftIO $ hPutStrLn stderr $ "[Generator] No user messages found."
            return $ Left "No user messages found."
        _ -> do
            let req = mkChatRequest chatRecords (agModel $ fmGeneratorConfig feThread) generatorParams
            resp <- lmRequest (T.unpack $ cfgLmEndpoint config) req
            case resp of
                Left err -> do
                    liftIO $ hPutStrLn stderr $ "[Generator] Error in LM request: " ++ err
                    return $ Left $ "Error in LM request: " ++ err
                Right chatResponse -> do
                    case maybeTopResponse chatResponse of
                        Just textResp -> do
                            let responseMessage = FrontEndMessage
                                    { fmMessageId = "assistant-" <> ftThreadId feThread
                                    , fmRole = "assistant"
                                    , fmMessage = textResp
                                    , fmRewriteFlag = False   -- No rewriting needed for the response
                                    }
                            return $ Right responseMessage
                        Nothing -> do
                            liftIO $ hPutStrLn stderr $ "[Generator] No response text found."
                            return $ Left "No response text found."

hdleRequest ::  Config -> Application
hdleRequest conf req resp = do

  -- 1. Read the lazy request body. It's lazy, so IO happens when consumed.
  body <- lazyRequestBody req

  case A.eitherDecode body :: Either String FrontEndThread of
    Left err -> do
        liftIO $ hPutStrLn stderr $ "[hdleRequest] Failed to decode request body: " ++ err
        resp $ responseLBS status400 [(hContentType, "text/plain; charset=utf-8")] "Invalid request body" -- Specify charset
    Right feThread -> do
        -- Log the received thread ID (optional)
        liftIO $ hPutStrLn stderr $ "[hdleRequest] Received thread ID: " ++ T.unpack (ftThreadId feThread)

        -- Inject system prompt into the thread
        let updatedThread = sysPromptInjector feThread conf

        -- Rewrite prompts in the thread
        rewrittenThread <- promptRewriter updatedThread conf

        case rewrittenThread of
            Left err -> do
                liftIO $ hPutStrLn stderr $ "[hdleRequest] Error in prompt rewriting: " ++ err
                resp $ responseLBS status500 [(hContentType, "text/plain; charset=utf-8")] "Internal Server Error" -- Specify charset
            Right finalThread -> do
                -- Generate response based on the rewritten thread
                responseMessage <- respGenerator finalThread conf

                case responseMessage of
                    Left err -> do
                        liftIO $ hPutStrLn stderr $ "[hdleRequest] Error in response generation: " ++ err
                        resp $ responseLBS status500 [(hContentType, "text/plain; charset=utf-8")] "Internal Server Error" -- Specify charset
                    Right message -> do
                        -- Respond with the generated message as JSON (or any other format you prefer)
                        let jsonResponse = A.encode message
                        resp $ responseLBS status200 [(hContentType, "application/json; charset=utf-8")] jsonResponse -- Specify charset


app :: Config      -- ^ Application configuration
    -> Application -- ^ Resulting WAI Application
app config request respond = do
    -- Log incoming request details (optional but helpful for debugging)
    liftIO $ hPutStrLn stderr $ "[app] Request received: " ++ show (requestMethod request) ++ " " ++ show (pathInfo request)
    -- Route based on method and path segments
    case (requestMethod request, pathInfo request) of
        -- Handle POST requests to /generate, passing config to the handler
        ("POST", ["generate"]) -> hdleRequest config request respond

        -- Simple health check endpoint
        ("GET", ["health"]) -> do
            liftIO $ hPutStrLn stderr "[app] Health check request received."
            respond $ responseLBS status200 [(hContentType, "text/plain; charset=utf-8")] "OK" -- Specify charset

        -- Optional: Respond nicely to root path requests
        (_, []) -> do
            liftIO $ hPutStrLn stderr "[app] Root path request received."
            respond $ responseLBS status200 [(hContentType, "text/plain; charset=utf-8")] "Service is running. Use POST /retrieval for document retrieval." -- Specify charset

        -- Optional: Fun teapot endpoint
        (_, ["teapot"]) -> do
            liftIO $ hPutStrLn stderr "[app] Teapot request received."
            respond $ responseLBS status418 [(hContentType, "text/plain; charset=utf-8")] "418 I'm a teapot" -- Specify charset

        -- Handle method not allowed for known paths with wrong method
        (_, ["retrieval"]) -> respond $ responseLBS status405 [(hContentType, "text/plain; charset=utf-8")] "Method Not Allowed (POST required for /retrieval)"
        (_, ["health"])    -> respond $ responseLBS status405 [(hContentType, "text/plain; charset=utf-8")] "Method Not Allowed (GET required for /health)"

        -- Handle anything else with 404 Not Found
        _ -> respond $ responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found"



main :: IO ()
main = do
    putStrLn "Starting Document Writer API service..."

    config <- loadConfig "config.yaml"
    case config of
        Left err -> putStrLn $ "Error loading config: " ++ show err
        Right cfg -> do
          printConfig cfg

          hPutStrLn stderr $ "Document Writer API service starting on port " ++ show ((ncPort . cfgNetwork) cfg) ++ "..."

          run ((ncPort . cfgNetwork) cfg) (app cfg)
