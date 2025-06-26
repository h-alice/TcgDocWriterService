{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-} -- Optional, can make response construction cleaner
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module VdbClient (
    retrievalDocument
    , RetrievalParameters(..)
    , RetrievalRequest(..)
    , RetrievalResponse(..)
    ) where

import GHC.Generics (Generic) -- For deriving Show, Eq
-- JSON Handling


-- Text and Bytestring
import Data.Text (Text)
-- For packing Content-Type header value
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq         as W
import qualified Data.Text            as T
import Control.Lens                 ((&), (.~), (^.)) -- For wreq operators
import Control.Exception            (try, SomeException)
import Data.Text.Encoding           (decodeUtf8') -- Use safe decoding for status msg
import Text.Printf                  (printf)
import Data.Aeson.Types (Parser)                                -- For custom parsing
import Data.Aeson                   ( FromJSON(..), ToJSON(..)  -- For JSON serial/deserialization
                                    , Value, withObject, object -- For JSON structure
                                    , (.=), (.:), (.!=), (.:?)  -- Operators
                                    )
import qualified Data.Aeson           as A    (encode, eitherDecode)                               
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)


data RetrievalParameters = RetrievalParameters
    { paramTopK :: !Int            -- ^ Optional: The number of top results to return. JSON key: "topK".
    , paramPoolSize :: !Int        -- ^ Optional: The number of documents to retrieve. JSON key: "poolSize".
    , paramAlpha :: !Double        -- ^ Optional: The alpha parameter for hybrid search. JSON key: "alpha".
    } deriving (Show, Generic)     -- Generic needed if using generic Aeson instances

data RetrievalRequest = RetrievalRequest
    { reqId  :: !Text            -- ^ User-provided request identifier. JSON key: "requestId".
    , reqCollection :: !Text     -- ^ The collection name to search in. JSON key: "collection".
    , reqQuery :: !Text
    , reqQueryParams :: !RetrievalParameters -- ^ The parameters for the retrieval request. JSON key: "query".
    } deriving (Show, Generic)   -- Generic needed if using generic Aeson instances

-- | Represents the JSON structure of the response to be sent.
data RetrievalResponse = RetrievalResponse
    { respId      :: !Text   -- ^ The request identifier, echoed back from the request. JSON key: "requestId".
    , respDocuments :: ![Text] -- ^ A list of retrieved document contents. JSON key: "documents".
    } deriving (Show, Generic)


-- ========================================================================== --
-- Aeson Instances for JSON Serialize and Deserialize                         --
-- ========================================================================== --

-- | RetrievalResponse from JSON.
instance FromJSON RetrievalParameters where
    parseJSON :: Value -> Parser RetrievalParameters
    parseJSON = withObject "RetrievalParameters" $ \v -> RetrievalParameters
        <$> v .:? "topK"     .!= 3
        <*> v .:? "poolSize" .!= 20
        <*> v .:? "alpha"    .!= 0.5

-- | RetrievalRequest from JSON.
instance FromJSON RetrievalRequest where
    parseJSON :: Value -> Parser RetrievalRequest
    parseJSON = withObject "RetrievalRequest" $ \v -> RetrievalRequest
        <$> v .: "requestId"
        <*> v .: "collection"
        <*> v .: "query"
        <*> v .:? "queryParams" .!=  RetrievalParameters { paramTopK = 3, paramPoolSize = 20, paramAlpha = 0.5 }

instance FromJSON RetrievalResponse where
    parseJSON :: Value -> Parser RetrievalResponse
    parseJSON = withObject "RetrievalResponse" $ \v -> RetrievalResponse
        <$> v .: "requestId"
        <*> v .: "documents"

-- | Serialize RetrievalRequest to JSON.
instance ToJSON RetrievalRequest where
    toJSON :: RetrievalRequest -> Value
    toJSON RetrievalRequest {reqId, reqCollection, reqQuery, reqQueryParams} = object
        [ "requestId" .= reqId
        , "collection" .= reqCollection
        , "query" .= reqQuery
        , "queryParams" .= reqQueryParams
        ]

-- | Serialize RetrievalParameters to JSON.
instance ToJSON RetrievalParameters where
    toJSON :: RetrievalParameters -> Value
    toJSON RetrievalParameters {paramTopK, paramPoolSize, paramAlpha} = object
        [ "topK" .= paramTopK
        , "poolSize" .= paramPoolSize
        , "alpha" .= paramAlpha
        ]

-- | Deserialize RetrievalResponse from JSON.
instance ToJSON RetrievalResponse where
    toJSON :: RetrievalResponse -> Value
    toJSON RetrievalResponse {respId, respDocuments} = object
        [ "requestId" .= respId
        , "documents" .= respDocuments
        ]

-- =========================================================================== --
-- | VDB query client function                                                 --
-- =========================================================================== --

secondsToMicro :: Int -> Int
secondsToMicro s = s * 1000000


retrievalDocument :: String           -- ^ The full URL of the reranker API endpoint (e.g., "http://host:port/v1/rerank").
                -> RetrievalRequest           -- ^ The user query.
                -> IO (Either String RetrievalResponse) -- ^ Error message or the successfully parsed 'RerankResponse'.

retrievalDocument endPoint query = do

    -- 1. Configure HTTP request options
    let opts = W.defaults & W.header "Content-Type" .~ ["application/json"]
                          & W.manager .~ Left (defaultManagerSettings { 
                                  managerResponseTimeout = responseTimeoutMicro $ secondsToMicro 300 } ) -- 5 minutes timeout

    -- 2. Perform the HTTP POST request, catching potential exceptions
    eResult <- try (W.postWith opts endPoint (A.encode query))
                :: IO (Either SomeException (W.Response BL.ByteString))

    -- 3. Process the result
    case eResult of
        Left networkErr -> pure $ Left (printf "HTTP request failed: %s" (show networkErr))
        Right resp -> do
            let statusCode = resp ^. W.responseStatus . W.statusCode
            let statusMsgBytes = resp ^. W.responseStatus . W.statusMessage
            let responseBody = resp ^. W.responseBody

            -- Check for successful HTTP status code (2xx)
            if statusCode >= 200 && statusCode < 300
                then -- Success: Try to parse the response body into RetrievalResponse
                    case A.eitherDecode responseBody of
                        Left jsonErr -> pure $ Left (printf "JSON decoding failed: %s\nRaw body: %s" jsonErr (show responseBody))
                        Right retrievalResp -> pure $ Right retrievalResp
                else -- Retrieval API got an error status
                    -- Safely decode status message for inclusion in error
                    let statusMsgText = case decodeUtf8' statusMsgBytes of
                                            Left _ -> "(non-utf8 status message)"
                                            Right t -> T.unpack t
                    in pure $ Left (printf "Retrieval API request failed with status %d: %s\nResponse body: %s"
                                        statusCode statusMsgText (show responseBody))