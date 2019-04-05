{-# LANGUAGE OverloadedStrings #-}
module Financial.PortfolioSpec (spec) where

import Data.Maybe (isNothing, fromJust)
import qualified Data.Time as Time
import qualified Data.Vector as V
import Test.Hspec (describe, it, shouldSatisfy, Spec)
import Test.QuickCheck (property, shuffle, (==>), Arbitrary (..))
import qualified Extra.Data.Vector as V
import Financial.Eod.Record (Record (..))
import Financial.Portfolio (portfolioOf, positions, Position (..), Portfolio)

import Financial.ArbitraryPortfolio ()
import Financial.Eod.ArbitraryRecord (RecordSequence (..))
import Financial.Symbol.ArbitraryTicker ()


spec :: Spec
spec = describe "Financial.Portfolio" $ do
  describe "portfolioOf" $ do
    it "returns Nothing for empty lists" $
      portfolioOf [] `shouldSatisfy` isNothing

    it "returns Nothing if any of the ticker histories is empty" $ property $
      \(OneEmpty r1 r2) t1 t2 ->
          isNothing $ portfolioOf [(t1, 1, r1), (t2, 1, r2)]

    it "returns Nothing if any ticker has a non-overlapping price history" $ property $
      \(RecordSequence r) t1 t2 ->
          length r >= 4 ==> isNothing $ portfolioOf [
                                          (t1, 1, V.toList $ V.slice 0 2 r),
                                          (t2, 1, V.toList $ V.slice 2 2 r)
                                        ]

    it "returns portfolio with histories aligned" $ property $
      \(RecordSequence r) t1 t2 -> length r >= 7 ==>
          (positions . fromJust $ portfolioOf [
            (t1, 1, V.toList $ V.slice 0 4 r),
            (t2, 1, V.toList $ V.slice 2 5 r)
          ]) == [
            Position { ticker = t1, quantity = 1, history = V.slice 2 2 r },
            Position { ticker = t2, quantity = 1, history = V.slice 2 2 r }
          ]

    it "removes records from all positions if they are missing from one or more positions" $ property $
      \(RecordSequence r) t1 t2 -> length r > 10 ==>
          (positions . fromJust $ portfolioOf [
            (t1, 1, V.toList $ V.dropAt 5 r),
            (t2, 1, V.toList r)
          ]) == [
            Position { ticker = t1, quantity = 1, history = V.dropAt 5 r },
            Position { ticker = t2, quantity = 1, history = V.dropAt 5 r }
          ]

  describe "Position" $
    it "can be compared for equality" $ property $
      \t q (RecordSequence x) -> Position { ticker = t, quantity = q, history = x } ==
                                    Position { ticker = t, quantity = q, history = x}

  describe "Portfolio" $
    it "can be compared for equality" $ property $
      \p -> p == (p :: Portfolio)


data OneEmpty = OneEmpty [Record] [Record]
  deriving Show


instance Arbitrary OneEmpty where
  arbitrary = uncurry OneEmpty <$> choices
    where
      choices = do
        r <- arbitrary
        tup . take 2 <$> shuffle [
                            [r, r { date = Time.addDays 1 $ date r }],
                            [],
                            []
                          ]
      tup [a,b] = (a,b)
      tup _ = undefined


