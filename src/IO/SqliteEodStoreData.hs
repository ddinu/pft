{-|
  State definition, initialization and cleanup for the SQLite data store.
-}
{-# LANGUAGE OverloadedStrings #-}
module IO.SqliteEodStoreData (StoreData (..), new, finalize) where

import qualified Data.Text as T
import qualified Database.SQLite3 as Sqlite
import System.FilePath ((</>))


-- | State used by the SQLite data store.
data StoreData = StoreData {
                    db :: Sqlite.Database
                  }


-- | Create new state for the SQLite store.
new :: FilePath -> IO StoreData
new storeRoot = do
  db' <- Sqlite.open . T.pack $ storeRoot </> "eod.sqlite"
  Sqlite.exec db' "CREATE TABLE IF NOT EXISTS eod \
                  \(\
                    \ticker TEXT NOT NULL,\
                    \timestamp INTEGER NOT NULL,\
                    \record BLOB NOT NULL,\
                    \PRIMARY KEY (ticker, timestamp)\
                  \)"
  return StoreData { db = db' }


-- | Cleanup the specified state.
finalize :: StoreData -> IO ()
finalize sd = Sqlite.close $ db sd
