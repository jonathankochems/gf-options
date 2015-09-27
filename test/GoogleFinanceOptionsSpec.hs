{-# LANGUAGE ScopedTypeVariables #-}
module GoogleFinanceOptionsSpec (main, spec) where

{-Imports for testing-}
import Test.Hspec
import Test.QuickCheck

{-Basic libraries-}
import Data.Char(isPrint)
import Data.Time.Calendar(fromGregorian)
import qualified Control.Exception as Except
import Control.Lens ((.~),(^.))
import Data.Either

{-Modules to test-}
import GoogleFinanceOptions.Query
import qualified GoogleFinanceOptions.Query as Query
import GoogleFinanceOptions.Types



shouldErrorWith comp error =
    do result <- Except.try $ comp `seq` print ()
       either 
         (\(e :: Except.ErrorCall) -> Just $ show e) 
         (const Nothing)  
         result `shouldBe` Just error

main :: IO ()
main = hspec spec

spec :: Spec 
spec = do types
          rawquery
          query

types :: Spec
types = 
  describe "GoogleFinanceOptions.Types" $ do
    it "should provide a Result type which tags errors with error messages" $
        do let goodResult = Ok "goodResult"
               badResult  = Error "something went wrong"
           isOk goodResult  `shouldBe` True
           isOk badResult   `shouldBe` False
           isError goodResult `shouldBe` False
           isError badResult  `shouldBe` True
           show goodResult  `shouldBe` "Ok \"goodResult\""
           fromResult goodResult `shouldBe` "goodResult"
           res <- Except.try $ fromResult badResult
           either 
              (\(e :: Except.ErrorCall) -> True) 
              (const False)  
              res `shouldBe` True
           res' <- Except.try . print $ fromError goodResult
           either 
              (\(e :: Except.ErrorCall) -> True) 
              (const False)  
              res' `shouldBe` True
    it "should provide a lense interface to a Call through Contract" $
        do let call = contractId          .~ "312277272890488" $
                      name                .~ "" $
                      symbol              .~ "AAPL150731C00085000" $
                      exchange            .~ "OPRA" $
                      price               .~ Nothing $
                      change              .~ Nothing $
                      bid                 .~ Just 37.8 $
                      ask                 .~ Just 40.1 $
                      openinterest        .~ 0 $
                      volume              .~ Nothing $
                      strike              .~ 85.0 $
                      expiry              .~ fromGregorian 2015 7 31 $
                      underlyingSpotprice .~ 124.5 $
                      uninitialisedCall
           call ^. contractId          `shouldBe` "312277272890488" 
           call ^. name                `shouldBe` "" 
           call ^. symbol              `shouldBe` "AAPL150731C00085000" 
           call ^. exchange            `shouldBe` "OPRA" 
           call ^. price               `shouldBe` Nothing 
           call ^. change              `shouldBe` Nothing 
           call ^. bid                 `shouldBe` Just 37.8 
           call ^. ask                 `shouldBe` Just 40.1 
           call ^. openinterest        `shouldBe` 0 
           call ^. volume              `shouldBe` Nothing 
           call ^. strike              `shouldBe` 85.0 
           call ^. expiry              `shouldBe` fromGregorian 2015 7 31 
           call ^. underlyingSpotprice `shouldBe` 124.5 
    it "should provide a lense interface to a Put through Contract" $
        do let put = contractId   .~ "1122822964373558" $
                      name         .~ "" $
                      symbol       .~ "AAPL150731P00080000" $
                      exchange     .~ "OPRA" $
                      price        .~ Nothing $
                      change       .~ Nothing $
                      bid          .~ Nothing $
                      ask          .~ Just 2.0e-2 $
                      openinterest .~ 0 $
                      volume       .~ Nothing $
                      strike       .~ 80.0 $
                      expiry       .~ fromGregorian 2015 7 31 $
                      underlyingSpotprice .~ 124.5  $
                      uninitialisedPut
           put ^. contractId          `shouldBe` "1122822964373558" 
           put ^. name                `shouldBe` "" 
           put ^. symbol              `shouldBe` "AAPL150731P00080000" 
           put ^. exchange            `shouldBe` "OPRA" 
           put ^. price               `shouldBe` Nothing 
           put ^. change              `shouldBe` Nothing 
           put ^. bid                 `shouldBe` Nothing 
           put ^. ask                 `shouldBe` Just 2.0e-2 
           put ^. openinterest        `shouldBe` 0 
           put ^. volume              `shouldBe` Nothing 
           put ^. strike              `shouldBe` 80.0 
           put ^. expiry              `shouldBe` fromGregorian 2015 7 31 
           put ^. underlyingSpotprice `shouldBe` 124.5  
    it "should provide a default Put and Call Contract" $
        do uninitialisedPut ^. contractId           `shouldBe` "" 
           uninitialisedPut ^. name                 `shouldBe` "" 
           uninitialisedPut ^. symbol               `shouldBe` "" 
           uninitialisedPut ^. exchange             `shouldBe` "" 
           uninitialisedPut ^. price                `shouldBe` Nothing 
           uninitialisedPut ^. change               `shouldBe` Nothing 
           uninitialisedPut ^. bid                  `shouldBe` Nothing 
           uninitialisedPut ^. ask                  `shouldBe` Nothing 
           uninitialisedPut ^. openinterest         `shouldBe` -1 
           uninitialisedPut ^. volume               `shouldBe` Nothing 
           uninitialisedPut ^. strike               `shouldBe` -1.0 
           uninitialisedPut ^. expiry               `shouldBe` fromGregorian 0 0 0 
           uninitialisedPut ^. underlyingSpotprice  `shouldBe` -1.0 
           uninitialisedCall ^. contractId          `shouldBe` "" 
           uninitialisedCall ^. name                `shouldBe` "" 
           uninitialisedCall ^. symbol              `shouldBe` "" 
           uninitialisedCall ^. exchange            `shouldBe` "" 
           uninitialisedCall ^. price               `shouldBe` Nothing 
           uninitialisedCall ^. change              `shouldBe` Nothing 
           uninitialisedCall ^. bid                 `shouldBe` Nothing 
           uninitialisedCall ^. ask                 `shouldBe` Nothing 
           uninitialisedCall ^. openinterest        `shouldBe` -1 
           uninitialisedCall ^. volume              `shouldBe` Nothing 
           uninitialisedCall ^. strike              `shouldBe` -1.0 
           uninitialisedCall ^. expiry              `shouldBe` fromGregorian 0 0 0 
           uninitialisedCall ^. underlyingSpotprice `shouldBe` -1.0 

query :: Spec
query = 
  describe "GoogleFinanceOptions.Query" $ do
    it "should successfully perform a Google Finance Option Query (provided HTTP requests are served)" $
        do res <- Except.try $ request "AAPL" Nothing
           either 
              (\(e :: Except.IOException) -> True) 
              isOk  
              res `shouldBe` True
    it "should successfully obtain all available expiration dates which are consistent with a Google Finance Option Query" $
        do wholequery <- Except.try $ request "AAPL" Nothing
           expiries   <- Except.try $ getExpiryDates "AAPL"
           either 
              (\(e :: Except.IOException) -> Nothing) 
              (Just . availableExpirations . fromResult)  
              wholequery `shouldBe` either (\(e :: Except.IOException) -> Nothing) Just expiries
    it "should successfully perform a Google Finance Option Query for a specific date (provided HTTP requests are served)" $
        do expiries <- Except.try $ getExpiryDates "AAPL"
           let exps = either (\(e :: Except.IOException) -> []) id expiries
           length exps >= 2 `shouldBe` True                      
           res <- Except.try . request "AAPL" . Just $ exps !! 1
           either 
              (\(e :: Except.IOException) -> True) 
              isOk  
              res `shouldBe` True


rawquery :: Spec
rawquery = 
  describe "GoogleFinanceOptions.Query" $ do
    it "should provide an error message when parsing a Google Finance Option Query fails" $ do
        let errormsg = fromError $ parseRawOptionQueryResult "foo"
        errormsg `shouldBe` "parsing of raw option chain data failed: \"\\\"stdin\\\" (line 1, column 1):\\nunexpected \\\"f\\\"\\nexpecting white space or \\\"{\\\"\""
    it "should extract the queried expiry date for a given Google Finance Option Query" $ do
        let r = fromResult $ parseRawOptionQueryResult sampleQuery
        expiryDateForQuery r `shouldBe` fromGregorian 2015 7 31
    it "should extract the available expiry dates from a given Google Finance Option Query" $ do
        let r = fromResult $ parseRawOptionQueryResult sampleQuery
        availableExpirations r `shouldBe` [fromGregorian 2015 7 31, fromGregorian 2015 8 7]
    it "should extract the all *put* options from a given Google Finance Option Query" $ do
        let put1 = contractId   .~ "1122822964373558" $
                   name         .~ "" $
                   symbol       .~ "AAPL150731P00080000" $
                   exchange     .~ "OPRA" $
                   price        .~ Nothing $
                   change       .~ Nothing $
                   bid          .~ Nothing $
                   ask          .~ Just 2.0e-2 $
                   openinterest .~ 0 $
                   volume       .~ Nothing $
                   strike       .~ 80.0 $
                   expiry       .~ fromGregorian 2015 7 31 $
                   underlyingSpotprice .~ 124.5  $
                   uninitialisedPut
            put2 = contractId   .~ "50583084231368" $
                   name         .~ ""  $
                   symbol       .~ "AAPL150731P00085000" $
                   exchange     .~ "OPRA" $
                   price        .~ Nothing $
                   change       .~ Nothing $
                   bid          .~ Nothing $
                   ask          .~ Just 2.0e-2 $
                   openinterest .~ 0 $
                   volume       .~ Nothing $
                   strike       .~ 85.0 $
                   expiry       .~ fromGregorian 2015 7 31 $
                   underlyingSpotprice .~ 124.5 $
                   uninitialisedPut
        let r     = fromResult $ parseRawOptionQueryResult sampleQuery
        puts r `shouldBe` [put1, put2]
        let badPut = head . puts . fromResult $ parseRawOptionQueryResult badSampleQuery
        (badPut ^. contractId)   `shouldErrorWith` "error processing JSValue String cid" 
        (badPut ^. name)         `shouldErrorWith` "error processing JSValue String name"
        (badPut ^. symbol)       `shouldErrorWith` "error processing JSValue String s"
        (badPut ^. exchange)     `shouldErrorWith` "error processing JSValue String e"
        (badPut ^. openinterest) `shouldErrorWith` "error processing JSValue Int oi"
        (badPut ^. strike)       `shouldErrorWith` "error processing JSValue Double strike"
        (badPut ^. volume)       `shouldBe`        Just 42

    it "should extract the all *call* options from a given Google Finance Option Query" $ do
        let call1 = contractId   .~ "829128893700243" $ 
                    name         .~ "" $ 
                    symbol       .~ "AAPL150731C00080000" $ 
                    exchange     .~ "OPRA" $ 
                    price        .~ Nothing $ 
                    change       .~ Nothing $ 
                    bid          .~ Just 42.85 $ 
                    ask          .~ Just 44.85 $ 
                    openinterest .~ 0 $ 
                    volume       .~ Nothing $ 
                    strike       .~ 80.0 $ 
                    expiry       .~ fromGregorian 2015 7 31 $ 
                    underlyingSpotprice .~ 124.5 $
                    uninitialisedCall
            call2 = contractId          .~ "312277272890488" $
                    name                .~ "" $
                    symbol              .~ "AAPL150731C00085000" $
                    exchange            .~ "OPRA" $
                    price               .~ Nothing $
                    change              .~ Nothing $
                    bid                 .~ Just 37.8 $
                    ask                 .~ Just 40.1 $
                    openinterest        .~ 0 $
                    volume              .~ Nothing $
                    strike              .~ 85.0 $
                    expiry              .~ fromGregorian 2015 7 31 $
                    underlyingSpotprice .~ 124.5 $
                    uninitialisedCall
        let r = fromResult $ parseRawOptionQueryResult sampleQuery
        calls r `shouldBe` [call1, call2]
        let badCall = head . calls . fromResult $ parseRawOptionQueryResult badSampleQuery
        (badCall ^. contractId)   `shouldErrorWith` "error processing JSValue String cid" 
        (badCall ^. name)         `shouldErrorWith` "error processing JSValue String name"
        (badCall ^. symbol)       `shouldErrorWith` "error processing JSValue String s"
        (badCall ^. exchange)     `shouldErrorWith` "error processing JSValue String e"
        (badCall ^. openinterest) `shouldErrorWith` "error processing JSValue Int oi"
        (badCall ^. strike)       `shouldErrorWith` "error processing JSValue Double strike"
        (badCall ^. volume)       `shouldBe`        Just 42
        (badCall ^. bid)          `shouldBe`        Just 42.00

    it "should extract the underlying asset id from a given Google Finance Option Query" $ do
        let r = fromResult $ parseRawOptionQueryResult sampleQuery
            badr = fromResult $ parseRawOptionQueryResult badSampleQuery
        underlying r `shouldBe` "22144"
        underlying badr `shouldErrorWith` "error processing JSValue String underlying_id"
    it "should extract the underlying spotprice from a given Google Finance Option Query" $ do
        let r = fromResult $ parseRawOptionQueryResult sampleQuery
        queryUnderlyingSpotprice r `shouldBe` 124.5

sampleQuery = "{\"calls\":[{\"cid\":\"829128893700243\",\"name\":\"\",\"s\":\"AAPL150731C00080000\",\"e\":\"OPRA\",\"p\":\"-\",\"c\":\"-\",\"b\":\"42.85\",\"a\":\"44.85\",\"oi\":\"0\",\"vol\":\"-\",\"strike\":\"80.00\",\"expiry\":\"Jul 31, 2015\"},{\"cid\":\"312277272890488\",\"name\":\"\",\"s\":\"AAPL150731C00085000\",\"e\":\"OPRA\",\"p\":\"-\",\"c\":\"-\",\"b\":\"37.80\",\"a\":\"40.10\",\"oi\":\"0\",\"vol\":\"-\",\"strike\":\"85.00\",\"expiry\":\"Jul 31, 2015\"}],\"puts\":[{\"cid\":\"1122822964373558\",\"name\":\"\",\"s\":\"AAPL150731P00080000\",\"e\":\"OPRA\",\"p\":\"-\",\"c\":\"-\",\"b\":\"-\",\"a\":\"0.02\",\"oi\":\"0\",\"vol\":\"-\",\"strike\":\"80.00\",\"expiry\":\"Jul 31, 2015\"},{\"cid\":\"50583084231368\",\"name\":\"\",\"s\":\"AAPL150731P00085000\",\"e\":\"OPRA\",\"p\":\"-\",\"c\":\"-\",\"b\":\"-\",\"a\":\"0.02\",\"oi\":\"0\",\"vol\":\"-\",\"strike\":\"85.00\",\"expiry\":\"Jul 31, 2015\"}],\"expirations\":[{\"y\":2015,\"m\":7,\"d\":31},{\"y\":2015,\"m\":8,\"d\":7}],\"expiry\":{\"y\":2015,\"m\":7,\"d\":31},\"underlying_id\":\"22144\",\"underlying_price\":124.5}"
badSampleQuery = "{\"calls\":[{\"p\":\"-\",\"c\":\"-\",\"b\":\"+42.00\",\"a\":\"44.85\",\"vol\":\"42\",\"expiry\":\"Jul 31, 2015\"},{\"cid\":\"312277272890488\",\"name\":\"\",\"s\":\"AAPL150731C00085000\",\"e\":\"OPRA\",\"p\":\"-\",\"c\":\"-\",\"b\":\"37.80\",\"a\":\"40.10\",\"oi\":\"0\",\"vol\":\"-\",\"strike\":\"85.00\",\"expiry\":\"Jul 31, 2015\"}],\"puts\":[{\"p\":\"-\",\"c\":\"-\",\"b\":\"-\",\"a\":\"0.02\",\"vol\":\"42\",\"expiry\":\"Jul 31, 2015\"},{\"cid\":\"50583084231368\",\"name\":\"\",\"s\":\"AAPL150731P00085000\",\"e\":\"OPRA\",\"p\":\"-\",\"c\":\"-\",\"b\":\"-\",\"a\":\"0.02\",\"oi\":\"0\",\"vol\":\"-\",\"strike\":\"85.00\",\"expiry\":\"Jul 31, 2015\"}],\"expirations\":[{\"y\":2015,\"m\":7,\"d\":31},{\"y\":2015,\"m\":8,\"d\":7}],\"expiry\":{\"y\":2015,\"m\":7,\"d\":31},\"underlying_price\":124.5}"


