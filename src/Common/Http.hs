{- |
  Make HTTP(S) requests.
-}
module Common.Http (
  Body (..),
  Request (..),
  StatusCode (..),
  ThrottleDuration
) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Time as Time


-- | HTTP body.
newtype Body = Body BSL.ByteString


-- | HTTP status code.
newtype StatusCode = StatusCode Word
  deriving (Eq, Show)


-- | Duration of request throttling.
type ThrottleDuration = Time.DiffTime


-- | Fetch data from some HTTP(S) URL.
class Monad m => Request m where
  -- | Retrieve status code and data from an URL.
  get :: String -> m (StatusCode, Body)

  -- | Delay all requests for the specified duration.
  throttle :: ThrottleDuration -> m ()
