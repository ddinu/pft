{-|
  Data library monad transformer stack and state initialization.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IO.Data (
  Data,
  Config (..),
  State (..),
  runData,
  getConfig,
  getState,
  putState
) where

import Control.Exception (bracket)
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Lazy as S
import Control.Monad.IO.Class (MonadIO)
import qualified Network.HTTP.Conduit as HTTP
import Common.Error (Error)
import Financial.Eod.AlphaVantage (HasAlphaVantageApiKey (..))
import IO.Config (Config(..))
import qualified IO.SqliteEodStoreData as StoreData


-- | Global internal state.
data State = State {
                storeData :: StoreData.StoreData,
                httpManager :: HTTP.Manager
              }


-- | Global data retrieving monad.
newtype Data a = Data { unData :: E.ExceptT Error (R.ReaderT Config (S.StateT State IO)) a }
  deriving (Monad, Applicative, Functor, MonadIO, E.MonadError Error)


-- | Run the data monad computations.
runData :: Config -> Data a -> IO (Either Error a)
runData conf data' = bracket
      (StoreData.new $ storeRoot conf)
      StoreData.finalize $
      \storeData' -> do
          httpMgr <- HTTP.newManager HTTP.tlsManagerSettings
          let state = State { storeData = storeData', httpManager = httpMgr }
          flip S.evalStateT state . flip R.runReaderT conf . E.runExceptT $ unData data'


-- | Retrieve the global configuration.
getConfig :: Data Config
getConfig = Data R.ask


-- | Retrieve the global state.
getState :: Data State
getState = Data S.get


-- | Set a new global state.
putState :: State -> Data ()
putState = Data . S.put


instance HasAlphaVantageApiKey Data where
  getAlphaVantageApiKey = alphaVantageApiKey <$> getConfig
