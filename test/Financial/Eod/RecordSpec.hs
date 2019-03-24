{-# LANGUAGE OverloadedStrings #-}
module Financial.Eod.RecordSpec (spec) where

import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import qualified Data.Time.Calendar as Time
import qualified Financial.Eod.Record as Record
import Common.Serialize


spec :: Spec
spec = describe "Financial.Eod.Record" $ do
  let record = Record.Record {
    Record.date = Time.fromGregorian 2019 2 11,
    Record.openingPrice = 12.3,
    Record.closingPrice = 23.4,
    Record.highPrice = 34.5,
    Record.lowPrice = 01.2,
    Record.volume = 123456
  }

  it "records can be serialized and deserialized" $ do
    let record' = deserialize $ serialize record
    record' `shouldBe` Right record

  it "records can be shown" $
    show record `shouldSatisfy` (not . null)
