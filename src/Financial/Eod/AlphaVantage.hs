{-|
  Get EOD data from Alpha Vantage.
-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, FlexibleContexts #-}
module Financial.Eod.AlphaVantage (
  fetchRecords,
  makeUrl,
  parseRecords,
  retryCount,
  HasAlphaVantageApiKey (..)
) where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Csv as CSV
import qualified Data.Vector as V
import qualified Data.Time as Time
import qualified Extra.Data.Time as Time
import Common.Error (Error (..))
import qualified Common.Http as Http
import qualified Financial.Eod.Record as Record
import Financial.Symbol.Ticker (Ticker)


-- | Typeclass for getting the API key.
class Monad m => HasAlphaVantageApiKey m where
  getAlphaVantageApiKey :: m String


-- | When the rate limit is enforced, we'll stop all requests for this duration.
throttleDuration :: Http.ThrottleDuration
throttleDuration = Time.secondsToDiffTime 50


-- | Number of times to retry failed requests.
retryCount :: Int
retryCount = 5


type FetchConstraint m = (MonadError Error m, Http.Request m, HasAlphaVantageApiKey m)


{-|
  Fetch the latest records in an unspecified order.

  The 'ticker' argument specifies which ticker you want and the 'numRecords'
  argument specifies how many records to retrieve. This function may retrieve
  more or fewer records than requested.

  The current day is included but may have partial data.
-}
fetchRecords :: FetchConstraint m => Ticker -> Word -> m [Record.Record]
fetchRecords ticker numRecords = do
    apiKey <- getAlphaVantageApiKey
    fetchUrl $ makeUrl apiKey ticker numRecords
  where
    fetchUrl :: FetchConstraint m => String -> m [Record.Record]
    fetchUrl = tryFetch retryCount

    tryFetch :: FetchConstraint m => Int -> String -> m [Record.Record]
    tryFetch 0 _ = throwError $ Error "Failed to retrieve records from AlphaVantage."
    tryFetch n url = do
      (status, Http.Body body) <- Http.get url
      -- AlphaVantage doesn't fail with an HTTP error status when they throttle
      -- clients. They fail with a JSON-encoded error message (for CSV requests
      -- too). The code checks the status code for future-proofing and the body
      -- to detect throttling with the current API behavior.
      if (status == Http.StatusCode 200) && not (BSC.null body) && (BSC.head body /= '{')
      then return $ parseRecords body
      else Http.throttle throttleDuration >> tryFetch (n - 1) url


-- | Return an URL that can be used to fetch records.
makeUrl :: String -> Ticker -> Word -> String
makeUrl apiKey ticker numRecords =
    "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&datatype=csv" &:
    "symbol" =: show ticker &:
    "apikey" =: apiKey &:
    "outputsize" =: outputsize numRecords
  where
    outputsize n = if n <= 100 then "compact" else "full"
    a &: b = a ++ "&" ++ b
    a =: b = a ++ "=" ++ b


-- | Parse all valid records from a CSV byte string.
parseRecords :: BSL.ByteString -> [Record.Record]
parseRecords = catMaybes . parseAll
  where
    parseAll :: BSL.ByteString -> [Maybe Record.Record]
    parseAll bytes = V.toList $ V.map parseOne (decode bytes)

    parseOne ( timestamp :: BSL.ByteString,
               openingPrice :: Float,
               highPrice :: Float,
               lowPrice :: Float,
               _ :: Float, -- closingPrice
               adjustedclosingPrice :: Float,
               volume :: Int,
               _ :: Float, -- dividendAmount
               _ :: Float -- splitCoefficient
              ) =
      flip fmap (Time.parseYMD $ BSL.toStrict timestamp) $ \date ->
        Record.Record {
          Record.date = date,
          Record.openingPrice = openingPrice,
          Record.closingPrice = adjustedclosingPrice,
          Record.highPrice = highPrice,
          Record.lowPrice = lowPrice,
          Record.volume = volume
        }

    decode bytes' = case CSV.decode CSV.HasHeader bytes' of
                      Left _ -> V.empty
                      Right v -> v
