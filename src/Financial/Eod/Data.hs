{-|
  Get EOD data from the data store or from an API.
-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Financial.Eod.Data (get, stitch, firstMissingDay) where

import Control.Monad.Except (MonadError (..))
import Data.Either (rights)
import Data.List (sortOn, (\\))
import Data.Maybe (fromJust, isJust, listToMaybe)
import qualified Data.Time as Time
import qualified Extra.Data.Time as Time
import Common.Error (Error)
import Common.Http (Request)
import Financial.Eod.AlphaVantage (fetchRecords, HasAlphaVantageApiKey)
import qualified Financial.Eod.Record as Record
import Financial.Eod.Store (findRecords, optionalRecordDate, writeRecord, OptionalRecord, Store)
import Financial.Symbol.Ticker (Ticker)


type GetConstraint m = (MonadError Error m, Request m, Store m, Time.HasTime m, HasAlphaVantageApiKey m)


-- | Get a all available records in a date range.
get :: GetConstraint m => Ticker -> Time.Range -> m [Record.Record]
get ticker timeRange = rights <$> do
    today <- Time.utctDay <$> Time.currentTime
    records <- sortRecords <$> findRecords ticker timeRange
    let missingDay = firstMissingDay timeRange (map optionalRecordDate records)

    if isJust missingDay &&
       (fromJust missingDay < Time.rangeEnd timeRange) &&
       (fromJust missingDay < today)
    then do
      let numRecords = fromIntegral $ Time.diffDays today (fromJust missingDay)
      -- We'll fetch more records than we need because we include weekends and
      -- bank holidays or other non-trading days, but that's OK.

      let empty = map Left $ Time.days timeRange
      fetched <- sortRecords . map Right <$> fetchRecords ticker numRecords
      let missing = stitch fetched empty
      writeMissing ticker today missing

      let keep = Time.inRange timeRange . optionalRecordDate
      return $ records ++ filter keep missing
    else
      return records

  where
    writeMissing :: GetConstraint m => Ticker -> Time.Day -> [OptionalRecord] -> m ()
    writeMissing ticker' today' = mapM_ (writeRecord ticker') . filter ((/=) today' . optionalRecordDate)

    sortRecords :: [OptionalRecord] -> [OptionalRecord]
    sortRecords = sortOn optionalRecordDate


{-|
  Merge two sorted lists into a sorted list, removing duplicates by discarding
  elements from the second list.
-}
stitch :: [OptionalRecord] -> [OptionalRecord] -> [OptionalRecord]
stitch [] [] = []
stitch [] b = b
stitch a [] = a
stitch (a:as) (b:bs)
  | optionalRecordDate a < optionalRecordDate b = a : stitch as (b:bs)
  | optionalRecordDate a == optionalRecordDate b = a : stitch as bs
  | otherwise = b : stitch (a:as) bs


{-|
  Return the first day that is missing from the list of days so that they
  don't cover the entire range.
-}
firstMissingDay :: Time.Range -> [Time.Day] -> Maybe Time.Day
firstMissingDay range [] = Just $ Time.rangeStart range
firstMissingDay range days = mismatch (Time.days range) days
  where
    mismatch as bs = listToMaybe (take 1 $ as \\ bs)
