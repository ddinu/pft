{-# LANGUAGE OverloadedStrings #-}
module Financial.PortfolioSpec (spec) where

import Data.Maybe (isNothing, fromJust)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Vector as V
import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import Financial.Eod.Record (Record (..))
import Financial.Portfolio (portfolioOf, positions, Position (..))
import Financial.Symbol.Ticker (toTicker)


spec :: Spec
spec = describe "Financial.Portfolio" $ do
  let tickers = flip map ['1'..'3'] $ fromJust . toTicker . T.pack . flip (:) "XYZ"
  let records = flip map [18..22] $ \d -> Record {
                                              date = Time.fromGregorian 2019 3 d,
                                              openingPrice = 123,
                                              closingPrice = 125,
                                              lowPrice = 122,
                                              highPrice = 126,
                                              volume = 99999
                                            }

  describe "portfolioOf" $ do
    it "returns Nothing for empty lists" $
      portfolioOf [] `shouldSatisfy` isNothing

    it "returns Nothing if any of the ticker histories is empty" $
      portfolioOf [
        (tickers !! 0, 1, records),
        (tickers !! 1, 1, [])
      ] `shouldSatisfy` isNothing

    it "returns Nothing if any ticker has a non-overlapping price history" $
      portfolioOf [
        (tickers !! 0, 1, [records !! 0, records !! 1]),
        (tickers !! 1, 1, [records !! 2, records !! 3])
      ] `shouldSatisfy` isNothing

    it "returns portfolio with histories aligned" $
      positions (fromJust $ portfolioOf [
        (tickers !! 0, 1, [records !! 0, records !! 1, records !! 2]),
        (tickers !! 1, 1, [records !! 1, records !! 2, records !! 3])
      ]) `shouldBe` [
        Position {
          ticker = tickers !! 0,
          quantity = 1,
          history = V.fromList [records !! 1, records !! 2]
        },
        Position {
          ticker = tickers !! 1,
          quantity = 1,
          history = V.fromList [records !! 1, records !! 2]
        }
      ]

    it "removes records from all positions if they are missing from one or positions" $
      positions (fromJust $ portfolioOf [
        (tickers !! 0, 1, [records !! 0, records !! 2]),
        (tickers !! 1, 1, [records !! 0, records !! 1, records !! 2])
      ]) `shouldBe` [
        Position {
          ticker = tickers !! 0,
          quantity = 1,
          history = V.fromList [records !! 0, records !! 2]
        },
        Position {
          ticker = tickers !! 1,
          quantity = 1,
          history = V.fromList [records !! 0, records !! 2]
        }
      ]
