-- |
-- Module      : LmClient
-- Description : Defines the client for interacting with a Large Language Model (LLM) service.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : AGPL-3.0
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- | This module provides a client for interacting with a Large Language Model (LLM)
-- | service that is compatible with the OpenAI Chat Completions API format.
-- | It defines data types for requests and responses, and functions for
-- | sending requests and processing responses.
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module LmClient (
      mkChatRequest       -- | Constructor for 'ChatRequest'.
    , defaultLmParam      -- | Default 'LmParameters' for generation.
    , LmParameters(..)    -- | Parameters for controlling LLM generation.
    , ChatRecord(..)      -- | A single message in a chat conversation.
    , ChatRequest(..)     -- | The request payload for the chat completion API.
    , ChatResponse(..)    -- | The response payload from the chat completion API.
    , ResponseChoice(..)  -- | A single generation choice within a 'ChatResponse'.
    , ChatUsage(..)       -- | Token usage statistics for a request.
    , lmRequest           -- | Sends a request to the LLM API endpoint.
    , maybeTopResponse    -- | Extracts the content from the top response choice.
) where

-- Base
import Control.Exception (try, SomeException) -- For catching HTTP exceptions
import GHC.Generics (Generic) -- For deriving Generic
import Text.Printf (printf) -- For formatted error messages

-- Data
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8') -- For safe decoding

-- Third-party Libraries
import Control.Lens ((&), (.~), (^.)) -- For wreq operators
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT (Parser)
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro) -- For timeout settings
import qualified Network.Wreq as W

-- | Parameters controlling the generation process of the language model.
data LmParameters = LmParameters
    { temp :: !Double              -- ^ Sampling temperature. Higher values make output more random.
    , topP :: !Double              -- ^ Nucleus sampling parameter.
    , frequencyPenalty :: !Double  -- ^ Penalizes new tokens based on their existing frequency in the text so far.
    , presencePenalty :: !Double   -- ^ Penalizes new tokens based on whether they appear in the text so far.
    } deriving (Show, Generic)

-- | Represents a single message in a chat conversation.
data ChatRecord = ChatRecord
    { crRole :: !T.Text      -- ^ The role of the message author (e.g., "system", "user", "assistant").
    , crContent :: !T.Text   -- ^ The content of the message.
    } deriving (Show, Generic)

-- | Represents a request to the chat completion API.
data ChatRequest = ChatRequest
    { rqMessages :: ![ChatRecord]    -- ^ A list of messages describing the conversation so far.
    , rqModel :: !T.Text             -- ^ The ID of the model to use for completion.
    , rqFrequencyPenalty :: !Double  -- ^ The frequency penalty.
    , rqPresencePenalty :: !Double   -- ^ The presence penalty.
    , rqTemperature :: !Double       -- ^ The sampling temperature.
    , rqTopP :: !Double              -- ^ The nucleus sampling parameter.
    } deriving (Show, Generic)


-- | Provides a default set of 'LmParameters'.
-- | temp = 0.7, topP = 0.9, frequencyPenalty = 0.0, presencePenalty = 0.0
defaultLmParam :: LmParameters
defaultLmParam = LmParameters
    { temp = 0.7
    , topP = 0.9
    , frequencyPenalty = 0.0
    , presencePenalty = 0.0
    }

-- | A smart constructor for creating a 'ChatRequest'.
mkChatRequest :: [ChatRecord]   -- ^ The list of chat messages.
              -> T.Text         -- ^ The model identifier.
              -> LmParameters   -- ^ The generation parameters.
              -> ChatRequest
mkChatRequest messages model LmParameters{temp, topP, frequencyPenalty, presencePenalty} =
    ChatRequest
        { rqMessages = messages
        , rqModel = model
        , rqFrequencyPenalty = frequencyPenalty
        , rqPresencePenalty = presencePenalty
        , rqTemperature = temp
        , rqTopP = topP
        }

instance ToJSON ChatRecord where
    toJSON :: ChatRecord -> Value
    toJSON ChatRecord{crRole, crContent} =
        object ["role" .= crRole, "content" .= crContent]

instance FromJSON ChatRecord where
    parseJSON :: Value -> AT.Parser ChatRecord
    parseJSON = withObject "ChatRecord" $ \v -> ChatRecord
        <$> v .: "role"
        <*> v .: "content"

instance ToJSON ChatRequest where
    toJSON :: ChatRequest -> Value
    toJSON ChatRequest{rqMessages, rqModel, rqFrequencyPenalty, rqPresencePenalty, rqTemperature, rqTopP} =
        object  [  "messages" .= rqMessages
                ,  "model" .= rqModel
                ,  "frequency_penalty" .= rqFrequencyPenalty
                ,  "presence_penalty" .= rqPresencePenalty
                ,  "temperature" .= rqTemperature
                ,  "top_p" .= rqTopP
                ]

-- | Represents a single choice in a chat completion response.
data ResponseChoice = ResponseChoice
    { rcIndex :: !Int            -- ^ The index of the choice in the list of choices.
    , rcFinishReason :: !T.Text  -- ^ The reason the model stopped generating tokens.
    , rcMessage :: !ChatRecord   -- ^ The message generated by the model.
    } deriving (Show, Generic)

-- | Represents the token usage statistics for a chat completion request.
data ChatUsage = ChatUsage
    { cuPromptTokens :: !Int     -- ^ The number of tokens in the prompt.
    , cuCompletionTokens :: !Int -- ^ The number of tokens in the generated completion.
    , cuTotalTokens :: !Int      -- ^ The total number of tokens used in the request.
    } deriving (Show, Generic)

-- | Represents the full response from the chat completion API.
data ChatResponse = ChatResponse
    { rpId :: !T.Text                 -- ^ A unique identifier for the chat completion.
    , rpObject :: !T.Text             -- ^ The object type, which is always "chat.completion".
    , rpCreated :: !Int               -- ^ The Unix timestamp (in seconds) of when the chat completion was created.
    , rpModel :: !T.Text              -- ^ The model used for the chat completion.
    , rpChoices :: ![ResponseChoice]  -- ^ A list of chat completion choices.
    , rpUsage :: !ChatUsage           -- ^ Usage statistics for the completion request.
    , rpSystemFingerprint :: !T.Text  -- ^ This fingerprint represents the backend configuration that the model runs with.
    } deriving (Show, Generic)

instance FromJSON ResponseChoice where
    parseJSON :: Value -> AT.Parser ResponseChoice
    parseJSON = withObject "ResponseChoice" $ \v -> ResponseChoice
        <$> v .: "index"
        <*> v .: "finish_reason"
        <*> v .: "message"

instance FromJSON ChatUsage where
    parseJSON :: Value -> AT.Parser ChatUsage
    parseJSON = withObject "ChatUsage" $ \v -> ChatUsage
        <$> v .:? "prompt_tokens"       .!= 0
        <*> v .:? "completion_tokens"   .!= 0
        <*> v .:? "total_tokens"        .!= 0

instance FromJSON ChatResponse where
    parseJSON :: Value -> AT.Parser ChatResponse
    parseJSON = withObject "ChatResponse" $ \v -> ChatResponse
        <$> v .:?   "id"        .!= ""
        <*> v .:?   "object"    .!= "" 
        <*> v .:?   "created"   .!= 0
        <*> v .:?   "model"     .!= ""
        <*> v .:    "choices"
        <*> v .:?   "usage"     .!= ChatUsage { cuPromptTokens = 0, cuCompletionTokens = 0, cuTotalTokens = 0 }
        <*> v .:?   "system_fingerprint" .!= ""

-- | Helper function to convert seconds to microseconds for timeout settings.
secondsToMicro :: Int -> Int
secondsToMicro s = s * 1000000

-- | Sends a 'ChatRequest' to the specified LLM API endpoint.
-- | It handles the HTTP POST request, including setting headers, timeout,
-- | and processing the response.
lmRequest :: String         -- ^ The full URL of the LLM API endpoint.
          -> ChatRequest    -- ^ The chat request data.
          -> IO (Either String ChatResponse) -- ^ An error message or the parsed 'ChatResponse'.
lmRequest endPoint chatRequest = do
    let opts = W.defaults & W.header "Content-Type" .~ ["application/json"]
                          & W.manager .~ Left (defaultManagerSettings { 
                                  managerResponseTimeout = responseTimeoutMicro $ secondsToMicro 300 } ) -- 5 minutes timeout
    response <- try (W.postWith opts endPoint (A.toJSON chatRequest)) :: IO (Either SomeException (W.Response BL.ByteString))
    case response of
        Left err -> return $ Left $ printf "LM request failed due to network error: %s" (show err)
        Right res -> do
            let status = res ^. W.responseStatus
            let statusCode = status ^. W.statusCode
            let statusMsgBytes = status ^. W.statusMessage
            let body = res ^. W.responseBody
            if statusCode >= 200 && statusCode < 300
                then
                    case A.eitherDecode body of
                        Left jsonErr -> return $ Left $ printf "LM response JSON decoding failed: %s\nRaw body: %s" jsonErr (show body)
                        Right chatResponse -> return $ Right chatResponse
                else do
                    let statusMsgText = case decodeUtf8' statusMsgBytes of
                                            Left _ -> "(non-utf8 status message)"
                                            Right t -> T.unpack t
                    return $ Left $ printf "LM request failed with status %d: %s\nResponse body: %s"
                                        statusCode statusMsgText (show body)

-- | Extracts the content of the first choice from a 'ChatResponse'.
-- | Returns 'Nothing' if there are no choices.
maybeTopResponse :: ChatResponse -> Maybe T.Text
maybeTopResponse ChatResponse{rpChoices} =
    case rpChoices of
        [] -> Nothing
        (x:_) -> Just $ crContent $ rcMessage x