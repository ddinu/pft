{-# LANGUAGE OverloadedStrings #-}
module Financial.Symbol.TickerSpec (spec) where

import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Financial.Symbol.Ticker
import Common.Serialize (serialize, deserialize)


spec :: Spec
spec = describe "Financial.Ticker" $ do
  describe "toTicker" $ do
    it "returns Nothing for the empty string" $
      toTicker "" `shouldBe` Nothing

    it "creates ticker" $
      toTicker "XYZ" `shouldSatisfy` isJust

    it "returns Nothing if the string is too long" $
      toTicker (T.replicate 256 "A") `shouldBe` Nothing

  describe "Ticker" $ do
    it "can be shown" $
      show (fromJust $ toTicker "XYZ") `shouldBe` "XYZ"

    it "can be compared for equality" $
      fromJust (toTicker "XYZ") == fromJust (toTicker "XYZ") `shouldBe` True

    it "can be compared for inequality" $
      fromJust (toTicker "XYZ") /= fromJust (toTicker "ABC") `shouldBe` True

    it "can be serialized and deserialized" $ do
      let ticker = fromJust $ toTicker "XYZ"
      let ticker' = deserialize $ serialize ticker
      ticker' `shouldBe` Right ticker
