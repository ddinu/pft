{-# OPTIONS_GHC -fno-warn-orphans #-}
module Financial.Eod.ArbitraryRecord (
  RecordList (..),
  RecordSequence (..)
) where

import qualified Data.Time as Time
import qualified Data.Vector as V
import Test.QuickCheck (sized, Arbitrary (..))
import Test.QuickCheck.Instances ()
import Financial.Eod.Record (Record (..))


instance Arbitrary Record where
  arbitrary = Record <$>
                arbitrary <*>
                arbitrary <*>
                arbitrary <*>
                arbitrary <*>
                arbitrary <*>
                arbitrary


newtype RecordSequence = RecordSequence (V.Vector Record)
  deriving Show


instance Arbitrary RecordSequence where
  arbitrary = do
      day <- arbitrary
      sized $ \n ->
        RecordSequence . V.fromList <$> mapM (newRecord day) [0..(fromIntegral n `mod` 1000)]
    where
      newRecord day i = do
        r <- arbitrary
        return $ r { date = Time.addDays i day }


newtype RecordList = RecordList [Record]
  deriving Show


instance Arbitrary RecordList where
  arbitrary = do
    rs <- arbitrary
    let (RecordSequence r) = rs
    return . RecordList $ V.toList r
