{-# LANGUAGE OverloadedStrings #-}
module Financial.Symbol.TickerSpec (spec) where

import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import Test.QuickCheck (property, (==>))
import Financial.Symbol.Ticker
import Common.Serialize (serialize, deserialize)

import Financial.Symbol.ArbitraryTicker ()


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

    it "can be compared for equality" $ property $
      \x -> x == (x :: Ticker)

    it "x /= y => show x /= show y" $ property $
      \x y -> x /= y ==> show (x :: Ticker) /= show (y :: Ticker)

    it "can be serialized and deserialized" $ property $
      \x -> deserialize (serialize x) == Right (x :: Ticker)
