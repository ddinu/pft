{-# OPTIONS_GHC -fno-warn-orphans #-}
module Financial.ArbitraryPortfolio () where

import Control.Monad (forM)
import Data.Maybe (fromJust, isJust)
import Test.QuickCheck (sized, suchThat, Arbitrary (..))
import Financial.Portfolio (portfolioOf, Portfolio)

import Financial.Eod.ArbitraryRecord (RecordList (..))
import Financial.Symbol.ArbitraryTicker ()


instance Arbitrary Portfolio where
  arbitrary = sized $ \n -> fromJust <$> suchThat (portfolioOf <$> do
                                records <- arbitraryRecords
                                forM [0..(n `mod` 100)] (\_ -> do
                                    ticker <- arbitrary
                                    weight <- arbitrary
                                    return (ticker, weight, records))) isJust
              where
                arbitraryRecords = do
                  rs <- suchThat arbitrary $ \(RecordList x) -> length x > 2
                  let (RecordList records) = rs
                  return records
