-----------------------------------------------------------------------------
-- |
-- Module      :  GoogleFinanceOptions.Query
-- Copyright   :  (c) Jonathan Kochems 2015
-- License     :  BSD3
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  nonportable
--
-- A module abstracting the JSON layer in Google Finance Option Chain requests.
--
-- A Google Finance Option Chain requests queries the URL
--
-- > http://www.google.com/finance/option_chain?q=XXXX&output=json&expy=YYYY&expm=MM&expd=DD
--
-- which returns information for all options on symbol XXXX expiring on DD-MM-YYYY as a string.
-- This module provides request functions, a data structure, and a parser for this output. Further functions to interact with the result are provided.
-----------------------------------------------------------------------------
module GoogleFinanceOptions.Query(
  -- * Queries
  RawOptionQueryResult, request, getExpiryDates, 
  -- * Inspecting RawOptionQueryResult
  expiryDateForQuery, availableExpirations, puts, calls, underlying, queryUnderlyingSpotprice, 
  -- * Low-level functions
  rawRequest, parseRawOptionQueryResult, gfinanceUrl ) where

import Control.Applicative ((<$>))
import Control.Lens ((.~))

import qualified Network.HTTP    as HTTP 
import qualified Network.Browser as Browser
import Data.Time.Calendar

import Text.JSON(resultToEither)
import Text.JSON.Types (JSValue(..), JSObject(..), get_field, fromJSString)
import Text.JSON.Parsec(runParser, spaces, many, string, digit, choice, try, CharParser(..))
import Data.Maybe(fromMaybe, isNothing, fromJust)

import Text.JSON.Permissive(decodePermissive)
import GoogleFinanceOptions.Types (Call, Put, uninitialisedCall, uninitialisedPut, Contract(..), Result(..), fromResult)
--import GoogleFinanceOptions.Types (Contract) 
import qualified GoogleFinanceOptions.Types as Types


{--------------------------------------------------------------------
  Queries
--------------------------------------------------------------------}
-- | The URL for Google Finance Option Chain queries
gfinanceUrl = "http://www.google.com/finance/option_chain"

-- | Requests the option chain for a given symbol.
-- 
-- > request symbol Nothing
--
-- This will retrieve the option chain for the closest expiry date.
--
-- > request symbol (Just $ fromGregorian YYYY MM DD)
--
-- This will retrieve the option chain for the expiry date YYYY-MM-DD.
request :: String -> Maybe Day -> IO (Result RawOptionQueryResult)
request symbol date = parseRawOptionQueryResult <$> rawRequest symbol date

-- | Retrieves all available expiry dates.
getExpiryDates :: String -> IO [Day]
getExpiryDates symbol = ( availableExpirations .  fromResult ) <$> request symbol Nothing 

-- | The function raw_request performs the low-level HTTP request to Google Finance.
rawRequest :: String -> Maybe Day -> IO String
rawRequest symbol date = do 
    (_, rsp) <- Browser.browse $ do
                  Browser.setOutHandler (const $ return ())
                  Browser.setAllowRedirects True -- handle HTTP redirects
                  Browser.request $ HTTP.getRequest url
    return $ HTTP.rspBody rsp
  where url_base      = gfinanceUrl ++ "?q=" ++ symbol ++ "&output=json"
        url_exp y m d = url_base ++ "&expy=" ++ y ++ "&expm=" ++ m ++ "&expd=" ++ d
        url | isNothing date = url_base
            | otherwise      = (\(y,m,d) -> url_exp (show y) (show m) (show d)) $ toGregorian $ fromJust date

{--------------------------------------------------------------------
  Raw Results
--------------------------------------------------------------------}
-- | A wrapper type that abstracts results from Google Finance. We use RawOptionQueryResult to abstract the resulting data structure of an option chain query to Google Finance.
data RawOptionQueryResult = RawOptionQueryResult{ unwrapRawOptionQueryResult :: JSObject JSValue }

-- | Parses the result of a Google Finance Option Chain query as a string and returns the resulting RawOptionQueryResult.
parseRawOptionQueryResult :: String -> Result RawOptionQueryResult
parseRawOptionQueryResult doc = either (\e -> Error $ "parsing of raw option chain data failed: " ++ show e) 
                                       ( Ok . RawOptionQueryResult ) 
                                  $ resultToEither $ decodePermissive doc

-- | Returns the expiry date that was used in the Google Finance Option Chain query.
expiryDateForQuery :: RawOptionQueryResult -> Day                                
expiryDateForQuery raw_oc = fromJSDate exp_obj
    where Just (JSObject exp_obj) = get_field jsonobject "expiry"
          jsonobject = unwrapRawOptionQueryResult raw_oc 

-- | Returns the available expiry dates that can be further queried for.
availableExpirations :: RawOptionQueryResult -> [Day]
availableExpirations raw_oc = map (fromJSDate . fromJSOBJECT) expirations_list
    where Just (JSArray expirations_list) = get_field jsonobject "expirations"
          jsonobject = unwrapRawOptionQueryResult raw_oc 

-- | Returns all put options that expire on the queried date
puts :: RawOptionQueryResult -> [Put]
puts raw_oc = map (putFromJSONPut (queryUnderlyingSpotprice raw_oc) . fromJSOBJECT) put_list
    where Just (JSArray put_list) = get_field jsonobject "puts"
          jsonobject = unwrapRawOptionQueryResult raw_oc 

-- | Returns all call options that expire on the queried date
calls :: RawOptionQueryResult -> [Call]
calls raw_oc = map (callFromJSONCall (queryUnderlyingSpotprice raw_oc) . fromJSOBJECT) call_list
    where Just (JSArray call_list) = get_field jsonobject "calls"
          jsonobject = unwrapRawOptionQueryResult raw_oc 

-- | Returns the id of that underlying 
underlying :: RawOptionQueryResult -> String
underlying raw_oc = fromMyString "underlying_id" $ get_field jsonobject "underlying_id"
     where jsonobject = unwrapRawOptionQueryResult raw_oc 

-- | Returns the spotprice of the underlying 
queryUnderlyingSpotprice :: RawOptionQueryResult -> Double
queryUnderlyingSpotprice raw_oc = fromRational p  ::Double
    where Just (JSRational _ p) = get_field jsonobject "underlying_price"
          jsonobject = unwrapRawOptionQueryResult raw_oc 


{--------------------------------------------------------------------
  Helper functions
--------------------------------------------------------------------}
-- Converts a JSON object representing a put to an object of type Put
putFromJSONPut spot jsonput = 
    contractId          .~ fromMyString "cid"  (get_field jsonput "cid") $
    name                .~ fromMyString "name" (get_field jsonput "name") $
    symbol              .~ fromMyString "s"    (get_field jsonput "s") $
    exchange            .~ fromMyString "e"    (get_field jsonput "e") $
    price               .~ fromMyDouble'       (get_field jsonput "p") $
    change              .~ fromMyDouble'       (get_field jsonput "c" ) $
    bid                 .~ fromMyDouble'       (get_field jsonput "b") $
    ask                 .~ fromMyDouble'       (get_field jsonput "a") $
    openinterest        .~ fromMyInt "vol"     (get_field jsonput "oi") $
    volume              .~ fromMyInt'          (get_field jsonput "vol") $
    strike              .~ fromMyDouble "strike" (get_field jsonput "strike") $
    expiry              .~ dateFromString (fromMyString "expiry" $ get_field jsonput "expiry") $
    underlyingSpotprice .~ spot $
    uninitialisedPut

-- Converts a JSON object representing a call to an object of type Call
callFromJSONCall spot jsonput = 
    contractId          .~ fromMyString "cid"  (get_field jsonput "cid") $
    name                .~ fromMyString "name" (get_field jsonput "name") $
    symbol              .~ fromMyString "s"    (get_field jsonput "s") $
    exchange            .~ fromMyString "e"    (get_field jsonput "e") $
    price               .~ fromMyDouble' (get_field jsonput "p") $
    change              .~ fromMyDouble' (get_field jsonput "c" ) $
    bid                 .~ fromMyDouble' (get_field jsonput "b") $
    ask                 .~ fromMyDouble' (get_field jsonput "a") $
    openinterest        .~ fromMyInt "vol" (get_field jsonput "oi") $
    volume              .~ fromMyInt' (get_field jsonput "vol") $
    strike              .~ fromMyDouble "strike" (get_field jsonput "strike") $
    expiry              .~ dateFromString (fromMyString "expiry" $ get_field jsonput "expiry") $
    underlyingSpotprice .~ spot $
    uninitialisedCall


-- converting a JSON object representing a date to type Day
fromJSDate jsdate = fromGregorian (fromJSRational y) (fromJSRational m) (fromJSRational d)
    where Just y = get_field jsdate "y"
          Just m = get_field jsdate "m"
          Just d = get_field jsdate "d"

-- Converts a Maybe JSValue representing a potential string to a String
fromMyString err = fromJSString . fromJSSTRING . fromMaybe (error $ "error processing JSValue " ++ err)

-- Converts a Maybe JSValue representing a potential double to a Double
fromMyDouble :: String -> Maybe JSValue -> Double 
fromMyDouble err = read . fromJSString . fromJSSTRING . fromMaybe (error err)

-- Converts a Maybe JSValue representing a potential int to an Int
fromMyInt :: String -> Maybe JSValue -> Int
fromMyInt err = read . fromJSString . fromJSSTRING . fromMaybe (error err)

-- Converts a Maybe JSValue representing a potential double to a Maybe Double
fromMyDouble' :: Maybe JSValue -> Maybe Double 
fromMyDouble' x = do y <- x 
                     let z = fromJSString $ fromJSSTRING y
                     if z == "-" || null z 
                       then fail "-"
                       else (
                          if head z == '+' then
                              return $ read $ tail z 
                          else
                              return $ read z 
                          )

-- Converts a Maybe JSValue representing a potential int to a Maybe Int                    
fromMyInt' :: Maybe JSValue -> Maybe Int 
fromMyInt' x = do y <- x 
                  let z = fromJSString $ fromJSSTRING y
                  if z == "-" 
                    then fail "-"
                    else return $ read z

-- unwrapping various JSValue constructors
fromJSRational (JSRational _ x) = floor x
fromJSSTRING (JSString x) = x
fromJSOBJECT (JSObject x) = x

-- converting a string representing a date in "MMM DD, YYYY" format to a date of type Day
dateFromString stringdate = either (\_ -> error "dateFromString") (\(y,m,d) -> fromGregorian y m d) $ runParser date_p () "stdin" stringdate
    where date_p = do m <- month_p
                      _ <- spaces
                      d <- day_p
                      _ <- string "," >> spaces
                      y <- year_p
                      return (y,m,d)
          month_p = choice [string_index x n | (x,n) <- zip ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"] ([1..12] :: [Int]) ]
          day_p :: CharParser () Int
          day_p   = do d <- many digit
                       return (read d :: Int)
          year_p :: CharParser () Integer
          year_p  = do y <- many digit
                       return (read y :: Integer)             
          string_index x n = do try $ string x 
                                return n 

