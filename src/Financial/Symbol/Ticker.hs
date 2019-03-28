{-|
  A financial instrument's ticker (this is a component of the instrument's
  symbol).
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Financial.Symbol.Ticker (
  Ticker,
  toTicker
) where

import Data.Serialize (Serialize (..))
import qualified Data.ByteString.Short as BSS
import qualified Data.Hashable as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


-- | Instrument ticker (i.e. AAPL, MSFT, AMZN).
newtype Ticker = Ticker BSS.ShortByteString
  deriving (Eq, H.Hashable)


instance Show Ticker where
  show (Ticker s) = T.unpack . TE.decodeUtf8 $ BSS.fromShort s


instance Serialize Ticker where
  put (Ticker t) = put t
  get = Ticker <$> get


-- | String to ticker
toTicker :: T.Text -> Maybe Ticker
toTicker s
  | T.null s = Nothing
  | T.length s < 256 = Just . Ticker . BSS.toShort . TE.encodeUtf8 $ T.toUpper s
  | otherwise = Nothing
