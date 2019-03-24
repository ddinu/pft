{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Financial.Eod.AlphaVantageSpec (spec) where

import Control.Monad (when)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State.Lazy as S
import qualified Common.Http as Http
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import qualified Data.Time.Calendar as Time
import Common.Error (Error (..))
import qualified Financial.Symbol.Ticker as Ticker
import qualified Financial.Eod.Record as Record
import qualified Financial.Eod.AlphaVantage as AlphaVantage


spec :: Spec
spec = describe "Financial.DataSource.AlphaVantage" $ do
  let ticker = fromJust $ Ticker.toTicker "XYZ"

  let record = Record.Record {
                    Record.date = Time.fromGregorian 2019 2 6,
                    Record.openingPrice = 107,
                    Record.closingPrice = 106.03,
                    Record.highPrice = 107.1,
                    Record.lowPrice = 105.53,
                    Record.volume = 20511397
                  }

  describe "makeUrl" $ do
    it "creates url for a compact data set" $ do
      let actual = AlphaVantage.makeUrl "foo" ticker 100
      let expected = "https://www.alphavantage.co/query?\
                      \function=TIME_SERIES_DAILY_ADJUSTED\
                      \&datatype=csv\
                      \&symbol=XYZ\
                      \&apikey=foo\
                      \&outputsize=compact"
      actual `shouldBe` expected

    it "creates url for a full data set" $ do
      let actual = AlphaVantage.makeUrl "foo" ticker 101
      let expected = "https://www.alphavantage.co/query?\
                      \function=TIME_SERIES_DAILY_ADJUSTED\
                      \&datatype=csv\
                      \&symbol=XYZ\
                      \&apikey=foo\
                      \&outputsize=full"
      actual `shouldBe` expected

  describe "parseRecords" $ do
    it "returns an empty list for an empty bytestring" $
      AlphaVantage.parseRecords "" `shouldBe` []

    it "returns an empty list for a malformed string" $
      AlphaVantage.parseRecords "foo\nbar" `shouldBe` []

    it "returns an empty list for just the header" $
      AlphaVantage.parseRecords (head rows) `shouldBe` []

    it "returns an empty list for single row with no header" $
      AlphaVantage.parseRecords (rows !! 1) `shouldBe` []

    it "skips malformed date" $ do
      let csv = head rows `BSL.append`
                "2019-22-06,107.0000,107.1000,105.5300,109.1300,106.0300,20511397,0.0000,1.0000"
      AlphaVantage.parseRecords csv `shouldBe` []

    it "parses valid record" $ do
      let csv = BSL.concat $ take 2 rows
      AlphaVantage.parseRecords csv `shouldBe` [record]

    it "parses multiple records" $ do
      let csv = foldl1 BSL.append rows
      length (AlphaVantage.parseRecords csv) `shouldBe` 2

  describe "fetchRecords" $ do
    it "returns error if the requested data doesn't exist" $ do
      let records = evalEnv [(200, BSL.empty)] $ AlphaVantage.fetchRecords ticker 10
      records `shouldSatisfy` isLeft

    it "returns single record" $ do
      let csv = BSL.concat $ take 2 rows
      let records = evalEnv [(200, csv)] $ AlphaVantage.fetchRecords ticker 10
      records `shouldBe` Right [record]

    it "retries on failure" $ do
      let csv = BSL.concat $ take 2 rows
      let records = evalEnv [(500, BSL.empty), (200, csv)] $ AlphaVantage.fetchRecords ticker 10
      records `shouldBe` Right [record]

    it "throws error on retry failure" $ do
      let env = replicate (AlphaVantage.retryCount + 1) (500, BSL.empty)
      evalEnv env (AlphaVantage.fetchRecords ticker 10) `shouldSatisfy` isLeft


rows :: [BSL.ByteString]
rows = [
    "timestamp,open,high,low,close,adjusted_close,volume,dividend_amount,split_coefficient\n",
    "2019-02-06,107.0000,107.1000,105.5300,109.1300,106.0300,20511397,0.0000,1.0000\n",
    "2019-02-05,106.0600,107.2700,105.9600,107.2200,107.2200,27325365,0.0000,1.0000\n"
  ]


type Responses = [(Word, BSL.ByteString)]


newtype Env a = Env { unEnv :: E.ExceptT Error (S.State Responses) a }
  deriving (Monad, Applicative, Functor, S.MonadState Responses, E.MonadError Error)


evalEnv :: Responses -> Env a -> Either Error a
evalEnv responses = flip S.evalState responses . E.runExceptT . unEnv


instance Http.Request Env where
  get url = do
    when (length url == 0) (error "URL is null") -- Force evaluation of 'url'
    s <- S.get
    if null s then return (Http.StatusCode 500, Http.Body BSL.empty)
    else do
      S.put $ tail s
      let (status, body) = head s
      return (Http.StatusCode status, Http.Body body)

  throttle t = return $! seq t ()


instance AlphaVantage.HasAlphaVantageApiKey Env where
  getAlphaVantageApiKey = return ""
