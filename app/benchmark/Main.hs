{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<**>))
import Criterion.Main (bench, bgroup, defaultMain, whnfIO)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Options.Applicative as O
import qualified FinancialData as Fin


defaultConfigFile :: FilePath
defaultConfigFile = "config.yaml"


optParser :: O.Parser String
optParser = O.strOption
               ( O.long "config"
              <> O.short 'C'
              <> O.metavar "CONFIG_FILE"
              <> O.showDefault
              <> O.value defaultConfigFile
              <> O.help "Configuration file location" )


run :: Integer -> T.Text -> IO [Fin.Record]
run numDays ticker = do
    configFile <- O.execParser $ O.info (optParser <**> O.helper) (
                              O.fullDesc <> O.header "benchmark - benchmark fetching EOD data" )
    config <- Fin.loadConfig configFile

    startDate <- Time.addDays (- fromIntegral numDays) . Time.utctDay <$> Time.getCurrentTime
    let range = Fin.Range startDate $ fromIntegral numDays

    fromRight undefined <$> (Fin.runData config $ Fin.getEod (fromJust $ Fin.toTicker ticker) range)


main :: IO ()
main = defaultMain [
          bgroup "list" [
            bench "10 MSFT" $ whnfIO (run 10 "MSFT"),
            bench "1k MSFT" $ whnfIO (run 1000 "MSFT"),
            bench "10k MSFT" $ whnfIO (run 10000 "MSFT")
          ]
        ]
