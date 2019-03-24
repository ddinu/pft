{-# LANGUAGE OverloadedStrings #-}
module Common.SerializeSpec (spec) where

import Data.Either (isLeft, fromLeft)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Common.Error (Error (..))
import Common.Serialize (serialize, deserialize)


spec :: Spec
spec = describe "Common.Serialize" $ do
  it "deserialize is the inverse of serialize" $ property $
    \x -> (deserialize . serialize) x == Right (x :: Int)

  it "deserialize failes for invalid bytestring" $
    isLeft (deserialize "X" :: Either Error Int) `shouldBe` True

  it "deserialization error contains message" $ do
    let r = fromLeft undefined (deserialize "X" :: Either Error Int)
    (length (show r) > 1) `shouldBe` True
