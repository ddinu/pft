{-# LANGUAGE OverloadedStrings #-}
module Common.SerializeSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (property)
import Common.Error (Error (..))
import Common.Serialize (serialize, deserialize)


spec :: Spec
spec = describe "Common.Serialize" $ do
  it "deserialize is the inverse of serialize" $ property $
    \x -> (deserialize . serialize) x == Right (x :: Int)

  it "deserialize fails for invalid bytestring" $
    (deserialize "XYZ" :: Either Error Int) `shouldSatisfy` isLeft

  it "deserialize error contains message" $
    length (show (deserialize "XYZ" :: Either Error Int)) `shouldSatisfy` (5 <)
