{-|
  Financial portfolio with positions and their histories.
-}
module Financial.Portfolio (portfolioOf, Position (..), Portfolio (..)) where

import Control.Monad (forM)
import qualified Data.Vector as V
import Financial.Eod.Record (Record (..))
import Financial.Symbol.Ticker (Ticker)


-- | Portfolio position.
data Position = Position {
  ticker :: Ticker,
  quantity :: Int,
  history :: V.Vector Record
} deriving (Show, Eq)


-- | Portfolio of positions and price histories.
newtype Portfolio = Portfolio [Position]
  deriving (Show, Eq)


{-|
  Create a portfolio from a list of (ticker, quantity, history) triples. The EOD
  histories are aligned to the shortest common period. If no common period
  exists, this function will return Nothing.
-}
portfolioOf :: [(Ticker, Int, [Record])] -> Maybe Portfolio
portfolioOf ps
    | null ps || any (null . records) ps = Nothing
    | otherwise =
        let rs = map records ps
            minDate = minCommonDate rs
            maxDate = maxCommonDate rs
        in Portfolio <$> forM ps (\(t, q, rs') -> Position t q . V.fromList <$> alignTo minDate maxDate rs')
  where
    alignTo minDate' maxDate' = notEmptyHistory . filter (\r -> date r >= minDate' && date r <= maxDate')
    minCommonDate = maximum . map (date . head)
    maxCommonDate = minimum . map (date . last)
    notEmptyHistory v | null v = Nothing | otherwise = Just v
    records (_, _, rs') = rs'
