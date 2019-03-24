{-# LANGUAGE OverloadedStrings #-}
module Extra.Data.TimeSpec where

import Data.Either (fromRight)
import qualified Data.Time as Time
import Test.Hspec (describe, it, shouldBe, Spec)
import Test.QuickCheck (property)
import Common.Serialize
import Extra.Data.Time


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
    let range = Range {
                  rangeStart = Time.fromGregorian 2017 5 21,
                  rangeSize = 5
                }

    it "rangeEnd returns one day past the end of the range" $
      rangeEnd range `shouldBe` Time.fromGregorian 2017 5 26

    it "can be shown" $
      null (show range) `shouldBe` False

    it "a ranges with the same start and size are equal" $ do
      let range' = Range {
                      rangeStart = rangeStart range,
                      rangeSize = rangeSize range
                    }
      (range' == range) `shouldBe` True


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
    let range = Range { rangeStart = Time.fromGregorian 2019 3 10, rangeSize = 5 }

    it "returns false for the day before the range" $
      inRange range (pred $ rangeStart range) `shouldBe` False

    it "returns false for the day after the range" $
      inRange range (rangeEnd range) `shouldBe` False

    it "returns true for the first day of the range" $
      inRange range (rangeStart range) `shouldBe` True

    it "returns true for the last day of thre range" $
      inRange range (pred $ rangeEnd range) `shouldBe` True

  it "Time.Day can be serialized" $ do
    let d = Time.fromGregorian 2015 5 10
    (fromRight undefined . deserialize . serialize) d `shouldBe` d
