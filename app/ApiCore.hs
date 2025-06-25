{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-} -- Optional, can make response construction cleaner
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module ApiCore () where



import GHC.Generics (Generic) -- For deriving Show, Eq
-- JSON Handling


-- Text and Bytestring
import Data.Text (Text)
-- For packing Content-Type header value
 -- For wreq operators
 -- Use safe decoding for status msg
import Data.Aeson.Types (Parser)                                -- For custom parsing
import Data.Aeson                   ( FromJSON(..)  -- For JSON serial/deserialization
                                    , Value, withObject -- For JSON structure
                                    , (.:)  -- Operators
                                    )

-- Sample
-- {
--     "user_query": "What is the weather like today?",
--     "req_id": "12345",
--     "vdb_config": {
--         "collection": "weather_data",
--         "top_k": 5,
--         "pool_size": 10,
--         "alpha": 0.5
--     },
--     "generator_config": {
--         "model": "gpt-3.5-turbo",
--         "temperature": 0.7,
--         "top_p": 0.9,
--         "frequency_penalty": 0.0,
--         "presence_penalty": 0.0
--     }
-- }

-- Data structure for the API request
data ApiGeneratorRequest = ApiGeneratorRequest
  { agModel :: Text
  , agTemperature :: Double
  , agTopP :: Double
  , agFrequencyPenalty :: Double
  , agPresencePenalty :: Double
  } deriving (Show, Generic)

data ApiVdbConfig = ApiVdbConfig
  { avCollection :: Text
  , avTopK :: Int
  , avPoolSize :: Int
  , avAlpha :: Double
  } deriving (Show, Generic)

data ApiRequest = ApiRequest
  { arUserQuery :: Text
  , arReqId :: Text
  , arVdbConfig :: ApiVdbConfig
  , arGeneratorConfig :: ApiGeneratorRequest
  } deriving (Show, Generic)

-- Aeson instances for JSON serialization/deserialization
instance FromJSON ApiGeneratorRequest where
  parseJSON :: Value -> Parser ApiGeneratorRequest
  parseJSON = withObject "ApiGeneratorRequest" $ \v -> ApiGeneratorRequest
    <$> v .: "model"
    <*> v .: "temperature"
    <*> v .: "top_p"
    <*> v .: "frequency_penalty"
    <*> v .: "presence_penalty"

instance FromJSON ApiVdbConfig where
  parseJSON :: Value -> Parser ApiVdbConfig
  parseJSON = withObject "ApiVdbConfig" $ \v -> ApiVdbConfig
    <$> v .: "collection"
    <*> v .: "top_k"
    <*> v .: "pool_size"
    <*> v .: "alpha"

instance FromJSON ApiRequest where
  parseJSON :: Value -> Parser ApiRequest
  parseJSON = withObject "ApiRequest" $ \v -> ApiRequest
    <$> v .: "user_query"
    <*> v .: "req_id"
    <*> v .: "vdb_config"
    <*> v .: "generator_config"


