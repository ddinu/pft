{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Financial.MPT.Compute (
  evalComputation,
  portfolio,
  weights,
  Computation
) where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Lazy as S
import qualified Data.HashMap.Strict as Map
import Financial.Portfolio (positions, Portfolio, Position (..))
import Financial.Symbol.Ticker (Ticker)


-- | Computation state.
data State = State {
  maybeWeights :: Maybe (Map.HashMap Ticker Double)
}


-- | Create a new state with default values.
newState :: State
newState = State Nothing


-- | Computation in a portfolio.
newtype Computation a = Computation {
  unComputation :: S.StateT State (R.Reader Portfolio) a
} deriving (Monad, Applicative, Functor)


-- | Evaluate a computation and return its result.
evalComputation :: Portfolio -> Computation a -> a
evalComputation portfolio' = flip R.runReader portfolio' .
                             flip S.evalStateT newState .
                             unComputation


-- | Mapping from tickers to weights for all portfolio positions.
weights :: Computation (Map.HashMap Ticker Double)
weights = do
    state <- getState
    case maybeWeights state of
      Just ws -> return ws
      Nothing -> do
        ws <- computeWeights
        putState state { maybeWeights = Just ws }
        return ws
  where
    computeWeights = do
      positions' <- positions <$> portfolio
      let total = fromIntegral . sum $ map quantity positions'
      return . Map.fromList $ map (\p -> (ticker p, (fromIntegral . quantity) p @/ total)) positions'

    _ @/ 0 = 0
    a @/ b = a / b


-- | Get the computation portfolio.
portfolio :: Computation Portfolio
portfolio = Computation R.ask


-- | Gt the computation state.
getState :: Computation State
getState = Computation S.get


-- | Replace the current computation state.
putState :: State -> Computation ()
putState = Computation . S.put
