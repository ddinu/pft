module Common.HttpSpec (spec) where

import Test.Hspec (describe, it, Spec)
import Test.QuickCheck (property, (==>))
import Common.Http (StatusCode (..))


spec :: Spec
spec = describe "Common.Http" $
  describe "StatusCode" $ do
    it "has Show" $ property $
      \x -> show (StatusCode x) == ("StatusCode " ++ show x)

    it "status codes with the same value are equal" $ property $
      \x -> StatusCode x == StatusCode x

    it "status codes with different values are different" $ property $
      \x y -> x /= y ==> StatusCode x /= StatusCode y
