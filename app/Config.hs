{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Config (
    NetworkConfig(..)
  , GeneratorConfig(..)
  , Config(..)
  , loadConfig
) where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

import Data.ByteString (ByteString)
import Control.Applicative

data NetworkConfig =
  NetworkConfig {
    ncHostname :: Text
  , ncPort     :: Int
  } deriving (Eq, Show)

data GeneratorConfig =
  GeneratorConfig {
    gcSystem :: Text
  , gcPromptTemplate :: Text
  , gcDocPlaceholder :: Text
  , gcUserPlaceholder :: Text
  } deriving (Eq, Show)

data Config =
  Config {
    cfgNetwork  :: NetworkConfig
  , cfgGenerator :: GeneratorConfig
  , cfgLmEndpoint :: Text
  , cfgVdbEndpoint :: Text
  } deriving (Eq, Show)

instance FromJSON NetworkConfig where
  parseJSON :: Y.Value -> Y.Parser NetworkConfig
  parseJSON (Y.Object v) =
    NetworkConfig <$>
    v .: "hostname" <*>
    v .: "port"
  parseJSON _ = fail "Expected Object for NetworkConfig value"

instance FromJSON GeneratorConfig where
  parseJSON :: Y.Value -> Y.Parser GeneratorConfig
  parseJSON (Y.Object v) =
    GeneratorConfig <$>
    v .: "system"         <*>
    v .: "promptTemplate" <*>
    v .: "docPlaceholder" <*>
    v .: "userPlaceholder"
  parseJSON _ = fail "Expected Object for GeneratorConfig value"

instance FromJSON Config where
  parseJSON :: Y.Value -> Y.Parser Config
  parseJSON (Y.Object v) =
    Config <$>
    v .: "network"    <*>
    v .: "generator"  <*>
    v .: "lmEndpoint" <*>
    v .: "vdbEndpoint"
  parseJSON _ = fail "Expected Object for Config value"


loadConfig :: FilePath -> IO (Either Y.ParseException Config)
loadConfig path = do
  Y.decodeFileEither path