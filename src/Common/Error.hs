{-|
  Generic error that can be thrown and has a message.
-}
module Common.Error (
  Error (..)
) where

import Control.Exception (Exception)


-- | Generic error with a message.
newtype Error = Error String
  deriving Eq


instance Show Error where
  show (Error message) = message

instance Exception Error
