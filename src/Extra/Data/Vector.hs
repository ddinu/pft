module Extra.Data.Vector (dropAt) where

import qualified Data.Vector as V


dropAt :: Int -> V.Vector a -> V.Vector a
dropAt i xs = let (x1, x2) = V.splitAt i xs in x1 V.++ V.tail x2
