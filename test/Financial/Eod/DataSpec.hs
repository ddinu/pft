{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
module Financial.Eod.DataSpec (spec) where

import Test.Hspec
import Control.Monad (forM, when)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State.Lazy as S
import Data.Either (fromRight)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import qualified Data.HashMap.Strict as Map
import Common.Error (Error (..))
import qualified Common.Http as Http
import qualified Extra.Data.Time as Time
import qualified Financial.Eod.AlphaVantage as AlphaVantage
import qualified Financial.Eod.Store as Store
import qualified Financial.Eod.Data as Data
import qualified Financial.Eod.Record as Record
import Financial.Symbol.Ticker (toTicker, Ticker)


spec :: Spec
spec = describe "Financial.Eod.Data" $ do
  let ticker = fromJust $ toTicker "AAPL"

  let record = Record.Record { -- Must match the record in alphaVantageData
                  Record.date = Time.fromGregorian 2019 2 14,
                  Record.openingPrice = 108,
                  Record.closingPrice = 107.03,
                  Record.highPrice = 108.1,
                  Record.lowPrice = 106.53,
                  Record.volume = 20511398
                }

  let env = TestEnv {
      getStoreEnv = Map.empty,
      getHttpEnv = BSL.empty,
      getDate = Time.fromGregorian 2019 2 15
    }

  describe "get" $ do
    it "fetches records if not found in the store" $ do
      let range = Time.Range { Time.rangeStart = Record.date record, Time.rangeSize = 1 }
      let env' = env { getHttpEnv = alphaVantageData }
      let records = runTestState env' $ Data.get ticker range
      records `shouldBe` [record]

    it "doesn't fetch record if it exists in the store" $ do
      let range = Time.Range { Time.rangeStart = Record.date record, Time.rangeSize = 1 }
      let env' = env { getHttpEnv = undefined }
      let records = runTestState env' $ do
                        Store.writeRecord ticker $ Right record
                        Data.get ticker range
      records `shouldBe` [record]

    it "doesn't fetch record if an empty one exists in the store" $ do
      let range = Time.Range { Time.rangeStart = Record.date record, Time.rangeSize = 1 }
      let env' = env { getHttpEnv = undefined }
      let records = runTestState env' $ do
                        Store.writeRecord ticker . Left $ Record.date record
                        Data.get ticker range
      records `shouldBe` []

    it "fetches records only once" $ do
      let range = Time.Range { Time.rangeStart = Record.date record, Time.rangeSize = 1 }
      let env' = env { getHttpEnv = alphaVantageData }
      let records = runTestState env' $ do
                      _ <- Data.get ticker range
                      S.modify $ \e -> e { getHttpEnv = undefined }
                      Data.get ticker range
      records `shouldBe` [record]

    it "doesn't store current date" $ do
      let range = Time.Range { Time.rangeStart = getDate env, Time.rangeSize = 1 }
      let env' = env { getDate = Record.date record }
      let env'' = execTestState env' $ Data.get ticker range
      Map.lookup (toKey ticker $ getDate env) (getStoreEnv env'') `shouldBe` Nothing

    it "can fetch and return records from storage" $ do
      let range = Time.Range { Time.rangeStart = Time.fromGregorian 2019 2 13, Time.rangeSize = 3 }
      let env' = env { getHttpEnv = alphaVantageData }
      let records = runTestState env' $ do
                      Store.writeRecord ticker . Right $ record { Record.date = Time.fromGregorian 2019 2 13 }
                      Data.get ticker range
      length records `shouldBe` 3

  describe "stitch" $ do
    let record1 = Right record
    let record2 = Right $ record { Record.date = Time.addDays 1 $ Record.date record }
    let record3 = Right $ record { Record.date = Time.addDays 2 $ Record.date record }

    let empty1 = Left $ Record.date record
    let empty2 = Left . Time.addDays 1 $ Record.date record
    let empty3 = Left . Time.addDays 2 $ Record.date record

    it "returns an empty list if both lists are empty" $
      Data.stitch [] [] `shouldBe` []

    it "returns the second list if the first one is empty" $
      Data.stitch [] [record1] `shouldBe` [record1]

    it "returns the first list if the second one is empty" $
      Data.stitch [record1] [] `shouldBe` [record1]

    it "returns first list if there are no gaps" $ do
      let merged = Data.stitch [record1, record2, record3]
                               [empty1, empty2, empty3]
      merged `shouldBe` [record1, record2, record3]

    it "fills gaps with elements from the second list" $ do
      let merged = Data.stitch [record1, record3]
                               [empty1, empty2, empty3]
      merged `shouldBe` [record1, empty2, record3]

    it "second list can have gaps" $ do
      let merged = Data.stitch [record1, record3]
                               [empty1, empty3]
      merged `shouldBe` [record1, record3]

    it "second list can be ahead of the first list" $ do
      let merged = Data.stitch [record2, record3]
                               [empty1, empty2]
      merged `shouldBe` [empty1, record2, record3]

    it "second list can be behind the first list" $ do
      let merged = Data.stitch [record1, record2]
                               [empty2, empty3]
      merged `shouldBe` [record1, record2, empty3]

  describe "firstMissingDay" $ do
    let day1 = Time.fromGregorian 2019 3 18
    let day2 = Time.fromGregorian 2019 3 19
    let day3 = Time.fromGregorian 2019 3 20
    let day4 = Time.fromGregorian 2019 3 21
    let day5 = Time.fromGregorian 2019 3 22

    let range = Time.Range {
                  Time.rangeStart = Time.fromGregorian 2019 3 18,
                  Time.rangeSize = 5
                }

    let range1 = range { Time.rangeSize = 1 }

    it "returns rangeStart for an empty list" $
      Data.firstMissingDay range [] `shouldBe` Just (Time.rangeStart range)

    it "returns Nothing for a complete range" $
      Data.firstMissingDay range [day1, day2, day3, day4, day5] `shouldBe` Nothing

    it "returns the first missing day in the sequence" $
      Data.firstMissingDay range [day1, day3, day4, day5] `shouldBe` Just day2

    it "returns rangeStart if first day is missing" $
      Data.firstMissingDay range [day2, day3, day4, day5] `shouldBe` Just day1

    it "returns rangeEnd - 1 if the last day is missing" $
      Data.firstMissingDay range [day1, day2, day3, day4] `shouldBe` Just day5

    it "returns the first missing day in a multiday gap" $
      Data.firstMissingDay range [day1, day4, day5] `shouldBe` Just day2

    it "returns the first missing day with multiple gaps" $
      Data.firstMissingDay range [day1, day3, day5] `shouldBe` Just day2

    it "returns Nothing for a complete, single day range" $
      Data.firstMissingDay range1 [day1] `shouldBe` Nothing

    it "returns rangeStart for an incomplete, single day range" $
      Data.firstMissingDay range1 [day2, day3] `shouldBe` Just day1


type StoreEnv = Map.HashMap String Store.OptionalRecord


toKey :: Ticker -> Time.Day -> String
toKey t d = show t ++ show d


type HttpEnv = BSL.ByteString


data TestEnv = TestEnv {
                  getStoreEnv :: StoreEnv,
                  getHttpEnv :: HttpEnv,
                  getDate :: Time.Day
                }


newtype TestState a = TestState { unTestState :: E.ExceptT Error (S.State TestEnv) a }
  deriving (Monad, Applicative, Functor, E.MonadError Error, S.MonadState TestEnv)


runTestState :: TestEnv -> TestState a -> a
runTestState env = fromRight undefined . flip S.evalState env . E.runExceptT . unTestState


execTestState :: TestEnv -> TestState a -> TestEnv
execTestState env = flip S.execState env . E.runExceptT . unTestState


instance Http.Request TestState where
  get url = do
    when (length url == 0) (error "URL is null") -- Force evaluation of 'url'
    body <- getHttpEnv <$> S.get
    return (Http.StatusCode 200, Http.Body body)

  throttle d = return $ seq d ()

instance Store.Store TestState where
  findRecords ticker range = do
    let days = Time.days range
    catMaybes <$> forM days (\d -> Map.lookup (toKey ticker d) . getStoreEnv <$> S.get)

  writeRecord ticker record = do
    state <- TestState S.get
    let key = toKey ticker $ Store.optionalRecordDate record
    let env = Map.insert key record (getStoreEnv state)
    let state' = state { getStoreEnv = env }
    S.put state'

instance Time.HasTime TestState where
  currentTime = TestState (flip Time.UTCTime 0 . getDate <$> S.get)

instance AlphaVantage.HasAlphaVantageApiKey TestState where
  getAlphaVantageApiKey = return "api key"


alphaVantageData :: BSL.ByteString
alphaVantageData =
  "timestamp,open,high,low,close,adjusted_close,volume,dividend_amount,split_coefficient\n\
  \2019-02-15,107.0000,107.1000,105.5300,109.1300,106.0300,20511397,0.0000,1.0000\n\
  \2019-02-14,108.0000,108.1000,106.5300,110.1400,107.0300,20511398,1.0000,1.1000\n"
