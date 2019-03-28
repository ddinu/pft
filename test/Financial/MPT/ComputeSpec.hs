module Financial.MPT.ComputeSpec (spec) where

import qualified Data.HashMap.Strict as Map
import Test.Hspec (describe, it, shouldBe, Spec)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Time as Time
import Financial.Eod.Record (Record (..))
import Financial.MPT.Compute (evalComputation, weights)
import Financial.Portfolio (portfolioOf)
import Financial.Symbol.Ticker (toTicker)


spec :: Spec
spec = describe "Financial.MPT.Compute" $ do
  let tickers = flip map ['1'..'3'] $ fromJust . toTicker . T.pack . flip (:) "XYZ"
  let records = flip map [18..22] $ \d -> Record {
                                              date = Time.fromGregorian 2019 3 d,
                                              openingPrice = 123,
                                              closingPrice = 125,
                                              lowPrice = 122,
                                              highPrice = 126,
                                              volume = 99999
                                            }
  let portfolio = fromJust $ portfolioOf [
                                (tickers !! 0, 200, records),
                                (tickers !! 1, 200, records),
                                (tickers !! 2, 400, records)
                              ]

  it "computation returns result" $
    evalComputation portfolio (return 25) `shouldBe` (25 :: Int)

  it "can retrieve position weights" $ do
    let expected = Map.fromList [
                      (tickers !! 0, 0.25),
                      (tickers !! 1, 0.25),
                      (tickers !! 2, 0.5)
                    ]
    evalComputation portfolio weights `shouldBe` expected
