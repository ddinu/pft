{-|
  EOD record definition.
-}
module Financial.Eod.Record (
  Record (..)
) where

import qualified Data.Time as Time
import Data.Serialize (Serialize (..))


-- | EOD market data for a single day.
data Record = Record {
  date :: !Time.Day,
  openingPrice :: !Float,
  closingPrice :: !Float,
  highPrice :: !Float,
  lowPrice :: !Float,
  volume :: !Int
} deriving (Show, Eq)


instance Serialize Record where
  put r = do
    put . Time.toGregorian $ date r
    put $ openingPrice r
    put $ closingPrice r
    put $ highPrice r
    put $ lowPrice r
    put $ volume r

  get = do
    (y, m, d) <- get
    openingPrice' <- get
    closingPrice' <- get
    highPrice' <- get
    lowPrice' <- get
    volume' <- get

    return Record {
      date = Time.fromGregorian y m d,
      openingPrice = openingPrice',
      closingPrice = closingPrice',
      highPrice = highPrice',
      lowPrice = lowPrice',
      volume = volume'
    }
