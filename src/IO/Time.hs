{-|
  Retrieve te current time in the Data monad.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IO.Time () where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Extra.Data.Time (HasTime (..))
import IO.Data (Data)


instance HasTime Data where
  currentTime = liftIO getCurrentTime
