{-# OPTIONS_GHC -fno-warn-orphans #-}
module Financial.Symbol.ArbitraryTicker () where

import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Test.QuickCheck (suchThat, Arbitrary (..), ASCIIString (..))
import Test.QuickCheck.Instances ()
import Financial.Symbol.Ticker (toTicker, Ticker)


instance Arbitrary Ticker where
  arbitrary = fromJust . toTicker <$> suchThat (T.pack . getASCIIString <$> arbitrary) (isJust . toTicker)
