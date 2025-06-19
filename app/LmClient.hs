
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




-- {'id': 'chatcmpl-eb9enbp55qub24ujwkg7i',
--  'object': 'chat.completion',
--  'created': 1750322475,
--  'model': 'gemma-3-4b-it@q4_k_m',
--  'choices': [{'index': 0,
--    'logprobs': None,
--    'finish_reason': 'stop',
--    'message': {'role': 'assistant',
--     'content': 'Okay, let\'s break down the crime rates in Singapore. It’s really important to understand that Singapore consistently ranks as one of the *safest* countries globally, and this reflects strongly in its crime statistics. However, "crime rate" can be measured differently, so I\'ll give you a breakdown based on several key metrics:\n\n**1. Overall Crime Rates (Compared to Other Nations):**\n\n* **Extremely Low:** Singapore consistently has one of the *lowest* rates of violent and property crimes per capita in the world. It’s often cited as being safer than many developed Western nations.\n* **Significantly Lower Than Global Average:**  For example, according to UNODC data (United Nations Office on Drugs and Crime), Singapore\'s homicide rate is dramatically lower than most countries – around 0.14 per 100,000 people compared to a global average of roughly 6.7 per 100,000.\n\n**2. Specific Crime Categories & Recent Statistics (as of late 2023/early 2024 - Note: Data can shift slightly):**\n\n* **Serious Crimes:** This is the most closely watched category and includes offenses like murder, robbery, burglary, theft, and serious drug offences.\n    * **2022:** There were 3,876 serious crimes reported.\n    * **2021:**  There were 4,095 serious crimes reported. (This is a slight increase from the previous year).\n    * **Trend:** While there\'s been a small uptick in recent years, it’s still exceptionally low compared to many countries. The number of serious crimes per 100,000 population is very low – around 1.3.\n\n* **Petty Crime:** This includes things like pickpocketing and minor theft. These are also relatively rare.\n* **Drug Offences:** Singapore has a very strict approach to drug offenses, and there\'s been an increase in detections and arrests related to this due to enforcement efforts. However, the *number* of reported cases remains lower than many other countries with similar populations.\n\n**3. Key Factors Contributing to Low Crime Rates:**\n\n* **Strict Laws & Enforcement:** Singapore has a robust legal system and consistently enforces its laws, including severe penalties for criminal activity.\n* **High Police Presence & Surveillance:** There’s a significant police presence throughout the country, combined with extensive CCTV surveillance (particularly in public areas).\n* **Social Stability & Economic Prosperity:**  Singapore boasts high levels of social cohesion, economic opportunity, and relatively low income inequality – factors that are often linked to lower crime rates.\n* **Community Policing:** The Singaporean Police Force emphasizes community engagement and partnerships.\n\n\n\n**4. Sources for Data (Important for Further Research):**\n\n* **Singapore Ministry of Home Affairs (MHA):** [https://www.mha.gov.sg/](https://www.mha.gov.sg/) – This is the official source for crime statistics and reports in Singapore.\n* **United Nations Office on Drugs and Crime (UNODC):** [https://dataunodc.org/](https://dataunodc.org/) - Provides comparative international crime data, including Singapore\'s figures.\n* **Singapore Police Force:** [https://www.police.gov.sg/](https://www.police.gov.sg/) – Offers information about crime prevention and safety measures.\n\n**Important Note on Definitions & Comparisons:**  It’s crucial to remember that “crime rate” can be calculated in different ways (e.g., per capita, per 100,000 population). Also, comparing Singapore\'s statistics to other countries requires careful consideration of differences in legal systems, cultural norms, and reporting practices.\n\n**In short:  Singapore has an incredibly low crime rate compared to almost every country in the world.**\n\n\nDo you want me to delve deeper into a specific aspect of this information, such as:\n\n*   Trends over time?\n*   Specific types of crimes (e.g., drug offenses)?\n*   Comparisons with other Southeast Asian countries?'}}],
--  'usage': {'prompt_tokens': 24, 'completion_tokens': 877, 'total_tokens': 901},
--  'stats': {},
--  'system_fingerprint': 'gemma-3-4b-it@q4_k_m'}

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