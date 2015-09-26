-----------------------------------------------------------------------------
-- |
-- Module      :  GoogleFinanceOptions.Types
-- Copyright   :  (c) Jonathan Kochems 2015
-- License     :  BSD3
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  nonportable
--
-- The types provided by Google Finance Options
-----------------------------------------------------------------------------
module GoogleFinanceOptions.Types ( 
    Contract(..),
    Put,
    uninitialisedPut,
    Call,
    uninitialisedCall,
    Result(..), isOk, isError, fromResult 
    ) where

import Control.Lens (makeLenses, Simple, Lens, lens)
import Data.Time.Calendar(Day, fromGregorian)

{--------------------------------------------------------------------
  Option Contracts
--------------------------------------------------------------------}
class Contract t where 
    contractId   :: Simple Lens t String
    name         :: Simple Lens t String
    symbol       :: Simple Lens t String
    exchange     :: Simple Lens t String
    price        :: Simple Lens t (Maybe Double)
    change       :: Simple Lens t (Maybe Double)
    bid          :: Simple Lens t (Maybe Double)
    ask          :: Simple Lens t (Maybe Double)
    openinterest :: Simple Lens t Int
    volume       :: Simple Lens t (Maybe Int)
    strike       :: Simple Lens t Double
    expiry       :: Simple Lens t Day
    underlyingSpotprice :: Simple Lens t Double

data Put = Put{
    _putContractId :: String,
    _putName :: String,
    _putSymbol :: String,
    _putExchange :: String,
    _putPrice :: Maybe Double, 
    _putChange :: Maybe Double,
    _putBid :: Maybe Double,
    _putAsk :: Maybe Double,
    _putOpeninterest :: Int,
    _putVolume :: Maybe Int,
    _putStrike :: Double,
    _putExpiry :: Day,
    _putUnderlyingSpotprice :: Double
} deriving (Show, Eq)

putContractId :: Simple Lens Put String
putContractId = lens _putContractId (\p x -> p{ _putContractId = x})

putName :: Simple Lens Put String
putName = lens _putName (\p x -> p{ _putName = x})

putSymbol :: Simple Lens Put String
putSymbol = lens _putSymbol (\p x -> p{ _putSymbol = x})

putExchange :: Simple Lens Put String
putExchange = lens _putExchange (\p x -> p{ _putExchange = x})

putPrice :: Simple Lens Put (Maybe Double)
putPrice = lens _putPrice (\p x -> p{ _putPrice = x})

putChange :: Simple Lens Put (Maybe Double)
putChange = lens _putChange (\p x -> p{ _putChange = x})

putBid :: Simple Lens Put (Maybe Double)
putBid = lens _putBid (\p x -> p{ _putBid = x})

putAsk :: Simple Lens Put (Maybe Double)
putAsk = lens _putAsk (\p x -> p{ _putAsk = x})

putOpeninterest :: Simple Lens Put Int
putOpeninterest = lens _putOpeninterest (\p x -> p{ _putOpeninterest = x})

putVolume :: Simple Lens Put (Maybe Int)
putVolume = lens _putVolume (\p x -> p{ _putVolume = x})

putStrike :: Simple Lens Put Double
putStrike = lens _putStrike (\p x -> p{ _putStrike = x})

putExpiry :: Simple Lens Put Day
putExpiry = lens _putExpiry (\p x -> p{ _putExpiry = x})

putUnderlyingSpotprice :: Simple Lens Put Double
putUnderlyingSpotprice = lens _putUnderlyingSpotprice (\p x -> p{ _putUnderlyingSpotprice = x})

uninitialisedPut = Put {
                    _putContractId = "",
                    _putName                = "",
                    _putSymbol              = "",
                    _putExchange            = "",
                    _putPrice               = Nothing, 
                    _putChange              = Nothing,
                    _putBid                 = Nothing,
                    _putAsk                 = Nothing,
                    _putOpeninterest        = -1,
                    _putVolume              = Nothing,
                    _putStrike              = -1.0,
                    _putExpiry              = fromGregorian 0 0 0,
                    _putUnderlyingSpotprice = -1.0
                    }

instance Contract Put where
    contractId   = putContractId
    name         = putName
    symbol       = putSymbol
    exchange     = putExchange
    price        = putPrice
    change       = putChange
    bid          = putBid
    ask          = putAsk
    openinterest = putOpeninterest
    volume       = putVolume
    strike       = putStrike
    expiry       = putExpiry
    underlyingSpotprice = putUnderlyingSpotprice

data Call = Call{
    _callContractId :: String,
    _callName :: String,
    _callSymbol :: String,
    _callExchange :: String,
    _callPrice :: Maybe Double, 
    _callChange :: Maybe Double,
    _callBid :: Maybe Double,
    _callAsk :: Maybe Double,
    _callOpeninterest :: Int,
    _callVolume :: Maybe Int,
    _callStrike :: Double,
    _callExpiry :: Day,
    _callUnderlyingSpotprice :: Double
} deriving (Show, Eq)

callContractId :: Simple Lens Call String
callContractId = lens _callContractId (\p x -> p{ _callContractId = x})

callName :: Simple Lens Call String
callName = lens _callName (\p x -> p{ _callName = x})

callSymbol :: Simple Lens Call String
callSymbol = lens _callSymbol (\p x -> p{ _callSymbol = x})

callExchange :: Simple Lens Call String
callExchange = lens _callExchange (\p x -> p{ _callExchange = x})

callPrice :: Simple Lens Call (Maybe Double)
callPrice = lens _callPrice (\p x -> p{ _callPrice = x})

callChange :: Simple Lens Call (Maybe Double)
callChange = lens _callChange (\p x -> p{ _callChange = x})

callBid :: Simple Lens Call (Maybe Double)
callBid = lens _callBid (\p x -> p{ _callBid = x})

callAsk :: Simple Lens Call (Maybe Double)
callAsk = lens _callAsk (\p x -> p{ _callAsk = x})

callOpeninterest :: Simple Lens Call Int
callOpeninterest = lens _callOpeninterest (\p x -> p{ _callOpeninterest = x})

callVolume :: Simple Lens Call (Maybe Int)
callVolume = lens _callVolume (\p x -> p{ _callVolume = x})

callStrike :: Simple Lens Call Double
callStrike = lens _callStrike (\p x -> p{ _callStrike = x})

callExpiry :: Simple Lens Call Day
callExpiry = lens _callExpiry (\p x -> p{ _callExpiry = x})

callUnderlyingSpotprice :: Simple Lens Call Double
callUnderlyingSpotprice = lens _callUnderlyingSpotprice (\p x -> p{ _callUnderlyingSpotprice = x})

uninitialisedCall = Call {
                    _callContractId = "",
                    _callName                = "",
                    _callSymbol              = "",
                    _callExchange            = "",
                    _callPrice               = Nothing, 
                    _callChange              = Nothing,
                    _callBid                 = Nothing,
                    _callAsk                 = Nothing,
                    _callOpeninterest        = -1,
                    _callVolume              = Nothing,
                    _callStrike              = -1.0,
                    _callExpiry              = fromGregorian 0 0 0,
                    _callUnderlyingSpotprice = -1.0
                    }

instance Contract Call where
    contractId   = callContractId
    name         = callName
    symbol       = callSymbol
    exchange     = callExchange
    price        = callPrice
    change       = callChange
    bid          = callBid
    ask          = callAsk
    openinterest = callOpeninterest
    volume       = callVolume
    strike       = callStrike
    expiry       = callExpiry
    underlyingSpotprice = callUnderlyingSpotprice


{--------------------------------------------------------------------
  Result Type
--------------------------------------------------------------------}
-- | Wrapper type for results
data Result a = Ok a | Error String 
    deriving Show

-- | checks that result is a non-error
isOk :: Result a -> Bool
isOk (Ok _ )       = True
isOk _             = False

-- | checks whether result is an error
isError :: Result a -> Bool
isError (Error _ ) = True
isError _          = False

-- | extracts result or throws an `error' if we have an error result 
fromResult :: Result a -> a
fromResult (Ok x)    = x
fromResult (Error s) = error s