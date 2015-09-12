-----------------------------------------------------------------------------
-- |
-- Module      :  GoogleFinanceOptions.Types
-- Copyright   :  (c) Jonathan Kochems 2015
-- License     :  TBD
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  nonportable
--
-- The types provided by Google Finance Options
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module GoogleFinanceOptions.Types ( 
    Contract(..),
    Put,
    uninitialisedPut,
    Call,
    uninitialisedCall,
    Result(..), isOk, isError, fromResult 
    ) where

import Data.Time.Calendar(Day, fromGregorian)
import Control.Lens (makeLenses, Simple, Lens)

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
makeLenses ''Put

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
makeLenses ''Call

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
