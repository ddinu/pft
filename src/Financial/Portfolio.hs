{-|
  Financial portfolio with positions and their histories.
-}
module Financial.Portfolio (
  portfolioOf,
  positions,
  Portfolio,
  Position (..)
) where

import qualified Data.HashSet as Set
import qualified Data.Vector as V
import Extra.Data.Time ()
import Financial.Eod.Record (Record (..))
import Financial.Symbol.Ticker (Ticker)


-- | Portfolio position.
data Position = Position {
  ticker :: !Ticker,
  quantity :: !Int,
  history :: !(V.Vector Record)
} deriving (Show, Eq)


-- | Portfolio of positions and price histories.
newtype Portfolio = Portfolio [Position]
  deriving (Show, Eq)


-- | Get the positions of a portfolio as a list.
positions :: Portfolio -> [Position]
positions (Portfolio ps) = ps


{-|
  Create a portfolio from a list of (ticker, quantity, history) triples.

  The EOD price histories of all positions are aligned to the shortest common
  period and any records that have dates that which are not common to all
  positions in the portfolio, are discarded.

  If no common period exists, this function returns Nothing. In other words
  all positions must have price histories for the same number of days and
  for at least one day.
-}
portfolioOf :: [(Ticker, Int, [Record])] -> Maybe Portfolio
portfolioOf ps
    | null ps || any (null . records) ps = Nothing
    | otherwise =
        let comDates = commonDates ps
        in Portfolio <$> ensureHistories (flip map ps $
              \(t, q, rs) -> Position {
                                ticker = t,
                                quantity = q,
                                history = commonRecords comDates rs
                              })
  where
    records (_, _, rs') = rs'
    commonDates ps' = foldl1 Set.intersection (flip map ps' $ Set.fromList . map date . records)
    commonRecords comDates' = V.fromList . filter (flip Set.member comDates' . date)
    ensureHistories ps'
      | any null (map history ps') = Nothing
      | otherwise = Just ps'
