{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM, forM_)
import Control.Applicative ((<**>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Options.Applicative as O
import qualified FinancialData as Fin


defaultConfigFile :: FilePath
defaultConfigFile = "config.yaml"


data Opts = Opts {
  configFile :: FilePath,
  numDays :: Int,
  tickers :: [String]
}


optParser :: O.Parser Opts
optParser = Opts
  <$> O.strOption
         ( O.long "config"
        <> O.short 'C'
        <> O.metavar "CONFIG_FILE"
        <> O.showDefault
        <> O.value defaultConfigFile
        <> O.help "Configuration file location" )
  <*> O.option O.auto
         ( O.long "days"
        <> O.short 'D'
        <> O.metavar "DAYS"
        <> O.help "Number of days of history (including weekends)" )
  <*> O.some (
        O.argument O.str
          ( O.metavar "TICKER [TICKER...]" <> O.help "Tickers to retrieve" )
        )


main :: IO ()
main = do
    opts <- O.execParser $ O.info (optParser <**> O.helper) (
                              O.fullDesc <> O.header "list - fetch EOD data" )
    config <- Fin.loadConfig (configFile opts)

    startDate <- Time.addDays (fromIntegral (- numDays opts)) . Time.utctDay <$> Time.getCurrentTime
    let tickers' = map parseTicker (tickers opts)
    let range = Fin.Range startDate . fromIntegral $ numDays opts

    let records = Fin.runData config .
                    forM tickers' $ \t -> do
                      rs <- Fin.getEod t range
                      return (t, rs)
    records >>= printRecords
  where
    parseTicker t = fromMaybe undefined (Fin.toTicker $ T.pack t)

    printRecords (Left e) = print e
    printRecords (Right rs) = forM_ rs $ \(ticker, values) -> do
                                print ticker
                                putStrLn $ intercalate "\n" (map show values)
                                putStrLn ""
