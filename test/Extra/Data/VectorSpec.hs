module Extra.Data.VectorSpec (spec) where

import qualified Data.Vector as V
import Test.Hspec (describe, it, Spec)
import Test.QuickCheck (property, (==>))
import Test.QuickCheck.Instances ()
import Extra.Data.Vector (dropAt)


spec :: Spec
spec = describe "Extra.Data.Vector" $
  it "dropAt removes element" $ property $
    \x y -> length x > 2 ==>
        dropAt (V.length x - 1) (x V.++ y) == V.init x V.++ (y :: V.Vector Int)
