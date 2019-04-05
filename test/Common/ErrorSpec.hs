module Common.ErrorSpec (spec) where

import Test.Hspec (describe, it, Spec)
import Test.QuickCheck (property, (==>))
import Common.Error


spec :: Spec
spec = describe "Common.Error" $
  describe "Error" $ do
    it "Show formats the message only" $ property $
      \x -> show (Error x) == x

    it "Errors with same message are equal" $ property $
      \x -> Error x == Error x

    it "Errors with different messages are not equal" $ property $
      \x y -> x /= y ==> Error x /= Error y
