{-# LANGUAGE OverloadedStrings #-}
module Financial.Eod.RecordSpec (spec) where

import Data.List (sort)
import Test.Hspec (describe, it, Spec)
import Test.QuickCheck (property)
import Financial.Eod.Record (Record (..))
import Common.Serialize

import Financial.Eod.ArbitraryRecord (RecordList (..))


spec :: Spec
spec = describe "Financial.Eod.Record" $ do
  it "records can be serialized and deserialized" $ property $
    \x -> deserialize (serialize x) == Right (x :: Record)

  it "records can be shown" $ property $
    \x -> not . null $ show (x :: Record)

  it "records can be sorted" $ property $
    \(RecordList x) -> map date (sort x) == sort (map date x)
