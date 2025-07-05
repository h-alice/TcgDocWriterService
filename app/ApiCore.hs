-- |
-- Module      : ApiCore
-- Description : (Genie API v1) Defines the core data types for the API request structure.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : AGPL-3.0
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the data types that correspond to the JSON structure 
-- of incoming API requests. It includes definitions for generator
-- configuration, vector database configuration, and the main request body,
-- along with their 'FromJSON' instances for parsing.
--
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApiCore (
    ApiGeneratorRequest(..)
  , ApiVdbConfig(..)
  , ApiRequest(..)
  , FrontEndMessage(..)
  , FrontEndThread(..)
) where

import Data.Aeson       (FromJSON (..), Value, withObject, (.:), (.=), (.:?), (.!=), ToJSON (..), object)
import Data.Aeson.Types (Parser)
import Data.Text        (Text)
import GHC.Generics     (Generic)

-- | Configuration for the text generator, corresponding to the `generator_config` JSON object.
data ApiGeneratorRequest = ApiGeneratorRequest
  { agModel            :: !Text   -- ^ The model to use (e.g., "gpt-3.5-turbo").
  , agTemperature      :: !Double -- ^ The temperature for sampling.
  , agTopP             :: !Double -- ^ The top-p sampling parameter.
  , agFrequencyPenalty :: !Double -- ^ The frequency penalty.
  , agPresencePenalty  :: !Double -- ^ The presence penalty.
  } deriving (Show, Generic)

-- | Configuration for the vector database, corresponding to the `vdb_config` JSON object.
data ApiVdbConfig = ApiVdbConfig
  { avCollection :: !Text   -- ^ The collection name to search in.
  , avTopK       :: !Int    -- ^ The number of top results to return.
  , avPoolSize   :: !Int    -- ^ The number of documents to retrieve from the wider pool.
  , avAlpha      :: !Double -- ^ The alpha parameter for hybrid search.
  } deriving (Show, Generic)

-- | Represents the main API request structure.
data ApiRequest = ApiRequest
  { arUserQuery       :: !Text               -- ^ The user's query text.
  , arReqId           :: !Text               -- ^ A unique identifier for the request.
  , arVdbConfig       :: !ApiVdbConfig       -- ^ Vector database configuration.
  , arGeneratorConfig :: !ApiGeneratorRequest -- ^ Text generator configuration.
  } deriving (Show, Generic)

-- | Represents a message received from the frontend, typically containing a user query and a flag for rewriting.
-- |   If `fmRewriteFlag` is True, it indicates that the query should be rewritten using prompt rewriter.
data FrontEndMessage = FrontEndMessage
  { fmMessageId    :: !Text
  , fmRole         :: !Text               -- ^ The role of the message sender (e.g., "user", "assistant").
  , fmMessage      :: !Text
  , fmRewriteFlag  :: !Bool
  } deriving (Show, Generic)

-- | A thread is a collection of messages, identified by a unique request ID.
data FrontEndThread = FrontEndThread
  { ftMessages        :: ![FrontEndMessage]  -- ^ List of messages in the thread.
  , fmVdbConfig       :: !ApiVdbConfig
  , fmGeneratorConfig :: !ApiGeneratorRequest
  , ftThreadId        :: !Text               -- ^ Unique identifier for the thread.
  } deriving (Show, Generic)

-- ========================================================================== --
-- Aeson Instances for JSON Deserialization                                   --
-- ========================================================================== --

-- | 'FromJSON' instance for 'ApiGeneratorRequest'.
instance FromJSON ApiGeneratorRequest where
  parseJSON :: Value -> Parser ApiGeneratorRequest
  parseJSON = withObject "ApiGeneratorRequest" $ \v -> ApiGeneratorRequest
    <$> v .: "model"
    <*> v .: "temperature"
    <*> v .: "top_p"
    <*> v .: "frequency_penalty"
    <*> v .: "presence_penalty"

-- | 'FromJSON' instance for 'ApiVdbConfig'.
instance FromJSON ApiVdbConfig where
  parseJSON :: Value -> Parser ApiVdbConfig
  parseJSON = withObject "ApiVdbConfig" $ \v -> ApiVdbConfig
    <$> v .: "collection"
    <*> v .: "top_k"
    <*> v .: "pool_size"
    <*> v .: "alpha"

-- | 'FromJSON' instance for 'ApiRequest'.
instance FromJSON ApiRequest where
  parseJSON :: Value -> Parser ApiRequest
  parseJSON = withObject "ApiRequest" $ \v -> ApiRequest
    <$> v .:  "user_query"
    <*> v .:? "thread_id"       .!= ""
    <*> v .:  "vdb_config"
    <*> v .:  "generator_config"

-- | 'FromJSON' instance for 'FrontEndMessage'.
instance FromJSON FrontEndMessage where
  parseJSON :: Value -> Parser FrontEndMessage
  parseJSON = withObject "FrontEndMessage" $ \v -> FrontEndMessage
    <$> v .:? "message_id"   .!= ""
    <*> v .:  "role"
    <*> v .:  "message"
    <*> v .:? "rewrite_flag" .!= False

-- | 'FromJSON' instance for 'FrontEndThread'.
instance FromJSON FrontEndThread where
  parseJSON :: Value -> Parser FrontEndThread
  parseJSON = withObject "FrontEndThread" $ \v -> FrontEndThread
    <$> v .: "messages"
    <*> v .: "vdb_config"
    <*> v .: "generator_config"
    <*> v .: "thread_id"

-- | 'ToJSON' instance for 'FrontEndMessage'.
instance ToJSON FrontEndMessage where
  toJSON :: FrontEndMessage -> Value
  toJSON FrontEndMessage {fmMessageId, fmRole, fmMessage, fmRewriteFlag} =
    object  [ "message_id" .= fmMessageId
            , "role" .= fmRole
            , "message" .= fmMessage
            , "rewrite_flag" .= fmRewriteFlag
            ]
