-- |
-- Module      : Main
-- Description : Main entry point for the TCG Document Writer Service.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : AGPL-3.0
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- This module serves as the main entry point for the TCG Document Writer Service.
-- It is responsible for loading configuration, setting up the web server (Warp),
-- and routing incoming API requests to the appropriate handlers for prompt rewriting
-- and LLM-based response generation.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

-- Base
import Control.Monad.IO.Class (liftIO)      -- To lift IO actions into the WAI Application monad
import System.IO              (hPutStrLn, stderr) -- For logging to standard error

-- Data
import qualified Data.Aeson   as A          -- For JSON encoding and decoding
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- Web Server
import Network.HTTP.Types        (status200, status204, status400, status404, status405, status418, status500) -- HTTP status codes
import Network.HTTP.Types.Header (hContentType, ResponseHeaders) -- Standard HTTP headers and types
import Network.Wai               (Application, lazyRequestBody, pathInfo, requestMethod, responseLBS) -- Core WAI types and functions
import Network.Wai.Handler.Warp  (run) -- The Warp web server

-- Internal Modules
import ApiCore   -- Core data types for the API request structure
import Config    -- Configuration loading and management
import LmClient  -- Client for the Large Language Model
import VdbClient -- Client for the Vector Database

-- Utils

-- | Converts a list of 'Either's into an 'Either' of a list.
-- | If any element is a 'Left', the result is the first 'Left'. Otherwise, it's a 'Right' of the list of values.
eitherListVerifier :: [Either String a] -> Either String [a]
eitherListVerifier [] = Right []
eitherListVerifier (x:xs) =
  case x of
    Left err -> Left err
    Right val ->
      case eitherListVerifier xs of
        Left err -> Left err
        Right vals -> Right (val:vals)

-- | Converts a 'FrontEndThread' into a list of 'ChatRecord's suitable for the LLM client.
chatRecordFromFeThread :: FrontEndThread -> [ChatRecord]
chatRecordFromFeThread FrontEndThread{ftMessages} =
    map (\FrontEndMessage{fmRole, fmMessage} -> ChatRecord
        { crRole = fmRole
        , crContent = fmMessage
        }) ftMessages

-- | Injects the system prompt from the configuration into the message thread.
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

-- | Rewrites a single user message if its 'fmRewriteFlag' is set.
-- | This involves retrieving relevant documents from the vector database and
-- | injecting them into a prompt template along with the original user query.
-- | Non-user messages or messages without the flag are returned unchanged.
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

          -- Separator
          let rwDocSep = (rwDocSeparator . gcRewriter . cfgGenerator) config
          -- retrievedDocs is a list of Text documents
          let docText = T.intercalate rwDocSep (respDocuments retrievedDocs)

          liftIO $ TIO.hPutStrLn stderr $ "[Rewriter] Retrieved documents: " <> docText

          -- Replace placeholders in the prompt template
          let rewrittenQuery =  T.replace docPlaceholder docText $
                                T.replace userPlaceholder fmMessage promptTemplate

          return $ Right FrontEndMessage{fmMessageId, fmRole, fmMessage = rewrittenQuery, fmRewriteFlag}

-- | Orchestrates the prompt rewriting process for an entire 'FrontEndThread'.
-- | It applies the 'rewriteWorker' to each message in the thread concurrently.
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

-- | Generates a response from the language model based on the provided thread.
-- | It constructs a request from the thread's messages and parameters, sends it to the LLM, and processes the response.
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


-- | Adds permissive CORS headers to a list of response headers.
addCorsHeaders :: ResponseHeaders -> ResponseHeaders
addCorsHeaders headers = headers ++ [   ("Access-Control-Allow-Origin", "*")
                                      , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
                                      , ("Access-Control-Allow-Headers", "Content-Type, Authorization") ]

-- | Handles CORS preflight (OPTIONS) requests by responding with a 204 status and appropriate headers.
hdlePreflight :: Application -- ^ The WAI Application for preflight requests.
hdlePreflight _req resp = do
  -- Log the preflight request (optional)
  liftIO $ hPutStrLn stderr "[hdlePreflight] Preflight request received"

  -- Respond with CORS headers
  resp $ responseLBS status204 (addCorsHeaders []) "Preflight response"


-- | The main request handler for the `/genie` endpoint.
-- | It decodes the request body, runs the prompt rewriting and response generation pipeline,
-- | and sends the final generated message back as a JSON response.
hdleRequest :: Config      -- ^ The application configuration.
            -> Application -- ^ The WAI Application for handling the request.
hdleRequest conf req resp = do

    -- 1. Read the lazy request body. It's lazy, so IO happens when consumed.
    body <- lazyRequestBody req

    case A.eitherDecode body :: Either String FrontEndThread of
      Left err -> do
        liftIO $ hPutStrLn stderr $ "[hdleRequest] Failed to decode request body: " ++ err
        resp $ responseLBS status400 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "Invalid request body" -- Specify charset
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
            resp $ responseLBS status500 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "Internal Server Error" -- Specify charset
          Right finalThread -> do
            -- Generate response based on the rewritten thread
            responseMessage <- respGenerator finalThread conf

            case responseMessage of
              Left err -> do
                liftIO $ hPutStrLn stderr $ "[hdleRequest] Error in response generation: " ++ err
                resp $ responseLBS status500 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "Internal Server Error" -- Specify charset
              Right message -> do
                -- Respond with the generated message as JSON (or any other format you prefer)
                let jsonResponse = A.encode message
                liftIO $ hPutStrLn stderr $ "[hdleRequest] Successfully generated response for thread: " ++ T.unpack (ftThreadId finalThread)
                resp $ responseLBS status200 (addCorsHeaders [(hContentType, "application/json; charset=utf-8")]) jsonResponse -- Specify charset


-- | The main WAI 'Application' for the server.
-- | It acts as a router, dispatching requests to the appropriate handler based on the request path and method.
app :: Config      -- ^ The application configuration.
    -> Application -- ^ Resulting WAI Application
app config request respond = do
  -- Log incoming request details (optional but helpful for debugging)
  liftIO $ hPutStrLn stderr $ "[app] Request received: " ++ show (requestMethod request) ++ " " ++ show (pathInfo request)
  -- Route based on method and path segments
  case (requestMethod request, pathInfo request) of

    -- Handle OPTIONS requests for CORS preflight
    ("OPTIONS", _) -> hdlePreflight request respond

    -- Handle POST requests to /generate, passing config to the handler
    ("POST", ["genie"]) -> hdleRequest config request respond

    -- Simple health check endpoint
    ("GET", ["health"]) -> do
      liftIO $ hPutStrLn stderr "[app] Health check request received."
      respond $ responseLBS status200 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "OK" -- Specify charset

    -- Optional: Respond nicely to root path requests
    (_, []) -> do
      liftIO $ hPutStrLn stderr "[app] Root path request received."
      respond $ responseLBS status200 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "Service is running. Use POST /genie for document writing." -- Specify charset

    -- Optional: Fun teapot endpoint
    (_, ["teapot"]) -> do
      liftIO $ hPutStrLn stderr "[app] Teapot request received."
      respond $ responseLBS status418 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "418 I'm a teapot" -- Specify charset

    -- Handle method not allowed for known paths with wrong method
    (_, ["genie"]) -> respond $ responseLBS status405 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "Method Not Allowed (POST required for /retrieval)"
    (_, ["health"])    -> respond $ responseLBS status405 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "Method Not Allowed (GET required for /health)"

    -- Handle anything else with 404 Not Found
    _ -> respond $ responseLBS status404 (addCorsHeaders [(hContentType, "text/plain; charset=utf-8")]) "Not Found"



-- | The main entry point for the application.
-- | It loads the configuration, prints it, and starts the Warp web server.
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
