{-# LANGUAGE OverloadedStrings #-}
module Extra.Data.TimeSpec where

import Data.Either (fromRight)
import qualified Data.Time as Time
import Test.Hspec (describe, it, shouldBe, Spec)
import Test.QuickCheck (property, (==>))
import Common.Serialize
import Extra.Data.Time

import Extra.Data.ArbitraryTime ()


spec :: Spec
spec = describe "Time" $ do
  it "day extracts the day from a date" $
    day (Time.fromGregorian 2029 10 23) `shouldBe` 23

  it "month extracts the month from a date" $
    month (Time.fromGregorian 2034 5 2) `shouldBe` 5

  it "year extracts the year from a date" $
    year (Time.fromGregorian 1923 1 5) `shouldBe` 1923

  describe "parseYMD" $ do
    it "parses valid date" $
      parseYMD "2017-05-21" `shouldBe` (Just $ Time.fromGregorian 2017 5 21)

    it "returns Nothing for invalid date" $
      parseYMD "2017-21-05" `shouldBe` Nothing

  it "toMicroseconds returns correct result" $ property $
    \x -> toMicroseconds (Time.secondsToDiffTime x) == (x * 1000 * 1000)

  describe "Range" $ do
    it "rangeEnd = rangeStart + rangeSize" $ property $
      \x -> rangeEnd x == Time.addDays (fromIntegral $ rangeSize x) (rangeStart x)

    it "can be shown" $ property $
      \x -> not . null $ show (x :: Range)

    it "ranges with the same start and size are equal" $ property $
      \x -> x == Range {
                    rangeStart = rangeStart x,
                    rangeSize = rangeSize x
                  }

    it "ranges with a different start are not equal" $ property $
      \x y -> rangeStart x /= rangeStart y ==> x /= y { rangeSize = rangeSize x}

    it "ranges with a different size are not equal" $ property $
      \x y -> rangeSize x /= rangeSize y ==> x /= y { rangeStart = rangeStart x}

    it "days returns all days in order" $ property $
      \x -> days x == [rangeStart x .. (Time.addDays (-1) $ rangeEnd x)]


  describe "notWeekend" $ do
    it "returns true for Monday" $
      notWeekend (Time.fromGregorian 2019 3 11) `shouldBe` True

    it "returns true for Tuesday" $
      notWeekend (Time.fromGregorian 2019 3 12) `shouldBe` True

    it "returns true for Wednesday" $
      notWeekend (Time.fromGregorian 2019 3 13) `shouldBe` True

    it "returns true for Thursday" $
      notWeekend (Time.fromGregorian 2019 3 14) `shouldBe` True

    it "returns true for Friday" $
      notWeekend (Time.fromGregorian 2019 3 15) `shouldBe` True

    it "returns false for Saturday" $
      notWeekend (Time.fromGregorian 2019 3 16) `shouldBe` False

    it "returns false for Sunday" $
      notWeekend (Time.fromGregorian 2019 3 17) `shouldBe` False

  describe "inRange" $ do
    it "inRange (rangeStart - 1) = False" $ property $
      \x -> not $ inRange x (Time.addDays (-1) $ rangeStart x)

    it "inRange rangeEnd = False" $ property $
      \x -> not $ inRange x (rangeEnd x)

    it "inRange rangeStart = True" $ property $
      \x -> rangeSize x > 0 ==> inRange x $ rangeStart x

    it "zero sized ranges cannot contain their rangeStart" $ property $
      \x -> not . inRange x { rangeSize = 0 } $ rangeStart x

    it "inRange (rangeEnd - 1) = True" $ property $
      \x -> rangeSize x > 0 ==> inRange x (Time.addDays (-1) $ rangeEnd x)

  it "Time.Day can be serialized" $ property $
    \x -> (fromRight undefined . deserialize . serialize) x == (x :: Time.Day)
