{-|
  Financial portfolio with positions and their histories.
-}
module Financial.Portfolio (portfolioOf, Position (..), Portfolio (..)) where

import Control.Monad (forM)
import qualified Data.Vector as V
import Financial.Eod.Record (Record (..))
import Financial.Symbol.Ticker (Ticker)


-- | Position and its price history.
data Position = Position {
  ticker :: Ticker,
  history :: V.Vector Record
} deriving Show


-- | Portfolio of positions and price histories.
newtype Portfolio = Portfolio [Position]
  deriving Show


{-|
  Create a portfolio from a list of ticker-history pairs. The EOD histories
  are aligned to the shortest common period and only tickers that have
  a common history are returned.

  Returns Nothing if there is no common history for any tickers.
-}
portfolioOf :: [(Ticker, [Record])] -> Maybe Portfolio
portfolioOf ps
    | null ps || any (null . snd) ps = Nothing
    | otherwise =
        let records = map snd ps
            minDate = minCommonDate records
            maxDate = maxCommonDate records
        in Portfolio <$> forM ps (\(t, rs) -> Position t . V.fromList <$> alignTo minDate maxDate rs)
  where
    alignTo minDate' maxDate' = notEmpty . filter (\r -> date r < minDate' && date r > maxDate')
    minCommonDate = maximum . map (date . head)
    maxCommonDate = minimum . map (date . last)
    notEmpty v | null v = Nothing | otherwise = Just v
