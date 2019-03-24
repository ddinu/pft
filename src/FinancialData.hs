{-|
  The financial data library allows easy retrieval of financial data from
  the persistent store or from the internet.
-}
module FinancialData (
  getEod,
  loadConfig,
  runData,
  toTicker,
  Config (..),
  Data,
  Error,
  Range (..),
  Record (..),
  Ticker,
) where

import Common.Error (Error (..))
import Extra.Data.Time (Range (..))
import IO.Data (runData, Data)
import IO.Http ()
import IO.SqliteEodStore ()
import IO.Time ()
import IO.Config (loadConfig, Config (..))
import qualified Financial.Eod.Data as EOD
import Financial.Eod.Record (Record (..))
import Financial.Symbol.Ticker (toTicker, Ticker)


{-|
  Retrieve EOD financial records for the specified ticker and the specified
  number of week days (including weekends).
-}
getEod :: Ticker -> Range -> Data [Record]
getEod = EOD.get
