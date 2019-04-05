module Financial.MPT.ComputeSpec (spec) where

import qualified Data.HashMap.Strict as Map
import Test.Hspec (describe, it, Spec)
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck (property)
import Financial.MPT.Compute (evalComputation, weights)
import Financial.Portfolio (positions, quantity, ticker)

import Financial.ArbitraryPortfolio ()


spec :: Spec
spec = describe "Financial.MPT.Compute" $ do
  modifyMaxSize (const 1) $ it "computation returns result" $ property $
    \p x -> evalComputation p (return x) == (x :: Int)

  it "can retrieve position weights" $ property $
    \p -> let total = fromIntegral . sum $ map quantity (positions p)
              safeDiv _ 0 = 0
              safeDiv a b = a / b
              expectedWeights = Map.fromList . flip map (positions p) $
                                    \x -> (
                                        ticker x,
                                        fromIntegral (quantity x) `safeDiv` total
                                      )
          in evalComputation p weights == expectedWeights
