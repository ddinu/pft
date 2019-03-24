{-|
  HTTP Request implementation in the Data monad.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IO.Http () where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import Common.Http (Request (..), Body (..), StatusCode (..))
import qualified Extra.Data.Time as Time
import IO.Data (Data, State (..), getState)


instance Request Data where
  get url = do
    manager <- httpManager <$> getState
    rq <- liftIO $ HTTP.parseRequest url
    response <- HTTP.httpLbs rq manager
    let status = fromIntegral . HTTP.statusCode $ HTTP.responseStatus response
    let body = HTTP.responseBody response
    return (StatusCode status, Body body)

  throttle = liftIO . threadDelay . fromIntegral . Time.toMicroseconds
