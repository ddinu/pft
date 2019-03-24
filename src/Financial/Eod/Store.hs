{-|
  EOD record store.
-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Financial.Eod.Store (
  findRecords,
  optionalRecordDate,
  writeRecord,
  OptionalRecord,
  Store
) where

import qualified Data.Time as Time
import qualified Financial.Eod.Record as Record
import Financial.Symbol.Ticker (Ticker)
import qualified Extra.Data.Time as Time


-- | A record might be missing if a day is not a trading day.
type OptionalRecord = Either Time.Day Record.Record


-- | Get an optional record's date
optionalRecordDate :: OptionalRecord -> Time.Day
optionalRecordDate (Left d) = d
optionalRecordDate (Right r) = Record.date r


-- | Persistent EOD record store.
class Monad m => Store m where
  -- | Read all records in a date range.
  findRecords :: Ticker -> Time.Range -> m [OptionalRecord]

  -- | Store a record.
  writeRecord :: Ticker -> OptionalRecord -> m ()
