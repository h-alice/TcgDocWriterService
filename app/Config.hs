{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Config (
    NetworkConfig(..)
  , GeneratorConfig(..)
  , Config(..)
  , RewriterConfig(..)
  , loadConfig
  , printConfig
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import System.IO              (hPutStrLn, stderr) -- For logging to standard error


data NetworkConfig =
  NetworkConfig {
    ncHostname :: Text
  , ncPort     :: Int
  } deriving (Eq, Show)

data RewriterConfig =
  RewriterConfig {
    rwPromptTemplate  :: Text
  , rwDocPlaceholder  :: Text
  , rwDocSeparator    :: Text
  , rwUserPlaceholder :: Text
  } deriving (Eq, Show)

data GeneratorConfig =
  GeneratorConfig {
    gcSystem :: Text
  , gcRewriter :: RewriterConfig
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

instance FromJSON RewriterConfig where
  parseJSON :: Y.Value -> Y.Parser RewriterConfig
  parseJSON (Y.Object v) =
    RewriterConfig <$>
    v .: "promptTemplate" <*>
    v .: "docPlaceholder" <*>
    v .: "docSeparator" <*> 
    v .: "userPlaceholder"
  parseJSON _ = fail "Expected Object for RewriterConfig value"

instance FromJSON GeneratorConfig where
  parseJSON :: Y.Value -> Y.Parser GeneratorConfig
  parseJSON (Y.Object v) =
    GeneratorConfig <$>
    v .: "system"   <*>
    v .: "rewriter"
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

loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
  loadResult <- Y.decodeFileEither path
  case loadResult of
    Left err -> return $ Left (Y.prettyPrintParseException err)
    Right cfg -> return $ Right cfg

printConfig :: Config -> IO ()
printConfig Config{..} = do
  TIO.hPutStrLn stderr "-------------------------------"
  TIO.hPutStrLn stderr "Configuration Loaded Successfully:"
  TIO.hPutStrLn stderr "-------------------------------"
  TIO.hPutStrLn stderr "Network Config: "
  TIO.hPutStrLn stderr $ "  Hostname: " <> ncHostname cfgNetwork
  TIO.hPutStrLn stderr $ "  Port: " <> T.pack (show $ ncPort cfgNetwork)
  TIO.hPutStrLn stderr $ "System Prompt: \n" <> gcSystem cfgGenerator
  TIO.hPutStrLn stderr $ "Prompt Template: \n" <> (rwPromptTemplate . gcRewriter) cfgGenerator
  TIO.hPutStrLn stderr $ "LM Endpoint: " <> cfgLmEndpoint
  TIO.hPutStrLn stderr $ "VDB Endpoint: " <> cfgVdbEndpoint
  TIO.hPutStrLn stderr "-------------------------------"