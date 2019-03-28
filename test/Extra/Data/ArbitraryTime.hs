{-# OPTIONS_GHC -fno-warn-orphans #-}
module Extra.Data.ArbitraryTime () where

import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()
import Extra.Data.Time (Range (..))


instance Arbitrary Range where
  arbitrary = Range <$> arbitrary <*> arbitrary
