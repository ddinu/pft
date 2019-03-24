{-|
  Utilities to work with dates, times and durations.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Extra.Data.Time (
  day,
  days,
  inRange,
  month,
  notWeekend,
  parseYMD,
  rangeEnd,
  toMicroseconds,
  year,
  HasTime (..),
  Range (..)
) where

import qualified Data.Ix as I
import qualified Data.ByteString as BS
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format ()
import Data.Time.Clock ()
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time


-- | Source of current time
class Monad m => HasTime m where
  currentTime :: m Time.UTCTime


-- | Parse a string that has the format: YYYY-MM-DD.
parseYMD :: BS.ByteString -> Maybe Time.Day
parseYMD =
  Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" . T.unpack . TE.decodeUtf8


-- | Extract the Gregorian day.
day :: Time.Day -> Int
day = day' . Time.toGregorian
  where
    day' (_, _, d) = d


-- | Extract the Gregorian month.
month :: Time.Day -> Int
month = month' . Time.toGregorian
  where
    month' (_, m, _) = m


-- | Extract the Gregorian year.
year :: Time.Day -> Integer
year = year' . Time.toGregorian
  where
    year' (y, _, _) = y


-- | Check if the specified day is not Saturday or Sunday.
notWeekend :: Time.Day -> Bool
notWeekend date = let (_, _, day') = Time.toWeekDate date
                  in day' < 6


-- | Convert a time difference to microseconds.
toMicroseconds :: Time.DiffTime -> Integer
toMicroseconds d = (Time.diffTimeToPicoseconds d `div` 1000) `div` 1000


-- | Date range.
data Range = Range { rangeStart :: Time.Day, rangeSize :: Word }
  deriving (Eq, Show)


-- | End of a range (one day past the end of the range).
rangeEnd :: Range -> Time.Day
rangeEnd r = Time.addDays (fromIntegral $ rangeSize r) (rangeStart r)


-- | True if the specified date is in the specified range.
inRange :: Range -> Time.Day -> Bool
inRange r d = d >= rangeStart r && d < rangeEnd r


-- | A list of all dates in the range.
days :: Range -> [Time.Day]
days r = I.range (rangeStart r, rangeEnd $ r { rangeStart = Time.addDays (-1) $ rangeStart r })


instance Serialize Time.Day where
  put d = put $ Time.toGregorian d
  get = (uncurry . uncurry) Time.fromGregorian <$> get
