{-|
  Binary serializable objects.
-}
module Common.Serialize (
  deserialize,
  serialize,
  Serialize -- From the cereal package.
) where

import Data.Serialize (Serialize, encode, decode)
import qualified Data.ByteString as BS
import Common.Error (Error (..))


-- | Serialize an object.
serialize :: Serialize a => a -> BS.ByteString
serialize = encode


-- | Restore a serialized object.
deserialize :: Serialize a => BS.ByteString -> Either Error a
deserialize = either (Left . Error . msg) Right . decode
  where
    msg :: String -> String
    msg s = "Deserialization failed: " ++ s
