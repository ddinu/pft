{-# LANGUAGE OverloadedStrings #-}
module IO.Config (
  loadConfig,
  Config (..)
) where

import Data.Yaml ((.:))
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Config as Yaml


-- | Global library configuration.
data Config = Config {
        alphaVantageApiKey :: String,
        storeRoot :: FilePath
      }


instance Yaml.FromJSON Config where
  parseJSON = Yaml.withObject "Data" $ \v -> Config
    <$> v .: "AlphaVantageApiKey"
    <*> v .: "StoreRoot"


-- | Load the configuration from a YAML file.
loadConfig :: FilePath -> IO Config
loadConfig file = Yaml.loadYamlSettings [file] [] Yaml.ignoreEnv
