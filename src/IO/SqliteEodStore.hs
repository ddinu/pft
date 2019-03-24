{-|
  SQLite based data store. See also, the SqliteStoreData and Data modules.
-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IO.SqliteEodStore () where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (liftEither)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Database.SQLite3 as Sqlite
import Common.Serialize (deserialize, serialize)
import qualified Extra.Data.Time as Time
import Financial.Eod.Store (Store (..), OptionalRecord, optionalRecordDate)
import IO.Data (Data, State (..), getState)
import qualified IO.SqliteEodStoreData as StoreData
import Common.Error (Error)


instance Store Data where
  findRecords ticker range = do
    db <- StoreData.db . storeData <$> getState
    mapM liftEither =<< liftIO (bracket
      (Sqlite.prepare db "SELECT record FROM eod WHERE ticker=?1 AND timestamp BETWEEN ?2 AND ?3")
      Sqlite.finalize
      (\st -> do
        Sqlite.bindSQLData st 1 . Sqlite.SQLText . T.pack $ show ticker
        Sqlite.bindSQLData st 2 . Sqlite.SQLInteger . fromIntegral . fromEnum $ Time.rangeStart range
        Sqlite.bindSQLData st 3 . Sqlite.SQLInteger . fromIntegral . fromEnum $ Time.rangeEnd range
        getResults st
      ))
    where
      getResults :: Sqlite.Statement -> IO [Either Error OptionalRecord]
      getResults st' =
        map deserialize <$> allResults st'

      allResults :: Sqlite.Statement -> IO [BS.ByteString]
      allResults st' = do
        result <- Sqlite.step st'
        if notDone result
        then do
          bytes <- Sqlite.columnBlob st' $ Sqlite.ColumnIndex 0
          rest <- allResults st'
          return (bytes:rest)
        else return []

      notDone Sqlite.Done = False
      notDone Sqlite.Row = True


  writeRecord ticker record = do
    db <- StoreData.db . storeData <$> getState
    _ <- liftIO $ bracket
      (Sqlite.prepare db "INSERT OR REPLACE INTO eod (ticker, timestamp, record) VALUES (?1, ?2, ?3)")
      Sqlite.finalize
      (\st -> do
        Sqlite.bindSQLData st 1 . Sqlite.SQLText . T.pack $ show ticker
        Sqlite.bindSQLData st 2 . Sqlite.SQLInteger . fromIntegral . fromEnum $ optionalRecordDate record
        Sqlite.bindSQLData st 3 . Sqlite.SQLBlob $ serialize record
        Sqlite.step st
      )
    return ()
