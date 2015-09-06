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
module GoogleFinanceOptions.Types where
--(export Result(..), isOk, isError, fromResult )
import Data.Time.Calendar(Day)

{--------------------------------------------------------------------
  Option Contracts
--------------------------------------------------------------------}
class Contract t where 
    contractId   :: t -> String
    name         :: t -> String
    symbol       :: t -> String
    exchange     :: t -> String
    price        :: t -> Maybe Double 
    change       :: t -> Maybe Double
    bid          :: t -> Maybe Double
    ask          :: t -> Maybe Double
    openinterest :: t -> Int
    volume       :: t -> Maybe Int
    strike       :: t -> Double
    expiry       :: t -> Day
    underlyingSpotprice :: t -> Double

data Put = Put{
    putContractId :: String,
    putName :: String,
    putSymbol :: String,
    putExchange :: String,
    putPrice :: Maybe Double, 
    putChange :: Maybe Double,
    putBid :: Maybe Double,
    putAsk :: Maybe Double,
    putOpeninterest :: Int,
    putVolume :: Maybe Int,
    putStrike :: Double,
    putExpiry :: Day,
    putUnderlyingSpotprice :: Double
} deriving (Show, Eq)

uninitialisedPut = error "not implemented" :: Put

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
    callContractId :: String,
    callName :: String,
    callSymbol :: String,
    callExchange :: String,
    callPrice :: Maybe Double, 
    callChange :: Maybe Double,
    callBid :: Maybe Double,
    callAsk :: Maybe Double,
    callOpeninterest :: Int,
    callVolume :: Maybe Int,
    callStrike :: Double,
    callExpiry :: Day,
    callUnderlyingSpotprice :: Double
} deriving (Show, Eq)

uninitialisedCall = error "not implemented" :: Call

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
