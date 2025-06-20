
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module LmClient (
      mkChatRequest
    , defaultLmParam
    , LmParameters(..)
    , ChatRecord(..)
    , ChatRequest(..)
    , ChatResponse(..)
    , ResponseChoice(..)
    , ChatUsage(..)

) where

import GHC.Generics (Generic) -- For deriving Show, Eq
import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as AT (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq         as W
import qualified Data.Text            as T
import Control.Lens                 ((&), (.~), (^.)) -- For wreq operators
import Control.Exception            (try, SomeException)
import Data.Text.Encoding           (decodeUtf8') -- Use safe decoding for status msg
import Text.Printf                  (printf)
import Data.Aeson                   ( FromJSON(..), ToJSON(..)
                                    , Value, object, (.=), (.:), (.:?) , (.!=)
                                    , withObject )
import Data.List                   (sortBy)
import Data.Ord                    (comparing)
import Network.Wai (Response)

data LmParameters = LmParameters
    { temp :: !Double       -- ^ Optional: The temperature for sampling.
    , topP :: !Double              -- ^ Optional: The top-p sampling parameter.
    , frequencyPenalty :: !Double  -- ^ Optional: The frequency penalty.
    , presencePenalty :: !Double   -- ^ Optional: The presence penalty.
    } deriving (Show, Generic)

data ChatRecord = ChatRecord
    { crRole :: !T.Text
    , crContent :: !T.Text
    } deriving (Show, Generic)

data ChatRequest = ChatRequest
    { rqMessages :: ![ChatRecord]    -- ^ The chat messages.
    , rqModel :: !T.Text             -- ^ The model to use for the chat.
    , rqFrequencyPenalty :: !Double  -- ^ The frequency penalty.
    , rqPresencePenalty :: !Double   -- ^ The presence penalty.
    , rqTemperature :: !Double       -- ^ The temperature for sampling.
    , rqTopP :: !Double              -- ^ The top-p sampling parameter.
    } deriving (Show, Generic)


defaultLmParam :: LmParameters
defaultLmParam = LmParameters
    { temp = 0.7
    , topP = 0.9
    , frequencyPenalty = 0.0
    , presencePenalty = 0.0
    }

mkChatRequest :: [ChatRecord] -> T.Text -> LmParameters -> ChatRequest
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
        object [ "messages" .= rqMessages
               , "model" .= rqModel
               , "frequency_penalty" .= rqFrequencyPenalty
               , "presence_penalty" .= rqPresencePenalty
               , "temperature" .= rqTemperature
               , "top_p" .= rqTopP
               ]
               
data ResponseChoice = ResponseChoice
    { rcIndex :: !Int
    , rcFinishReason :: !T.Text
    , rcMessage :: !ChatRecord
    } deriving (Show, Generic)

data ChatUsage = ChatUsage
    { cuPromptTokens :: !Int
    , cuCompletionTokens :: !Int
    , cuTotalTokens :: !Int
    } deriving (Show, Generic)

data ChatResponse = ChatResponse
    { rpId :: !T.Text
    , rpObject :: !T.Text
    , rpCreated :: !Int
    , rpModel :: !T.Text
    , rpChoices :: ![ResponseChoice]
    , rpUsage :: !ChatUsage
    , rpSystemFingerprint :: !T.Text
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