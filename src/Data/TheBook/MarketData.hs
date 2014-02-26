{-# LANGUAGE NullaryTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.MarketData
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Various data types from the point of a client listening
-- to order book updates.
-----------------------------------------------------------------------------
module Data.TheBook.MarketData (
    Side(..)
  , OrdType(..)
  , FirmQuote(..)
  , Priority(..)
) where

import Data.Word (Word32)
import qualified Data.TheBook.Types as Types
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)

data Side
  = Buy
  | Sell
  deriving (Eq, Show)
instance Arbitrary Side where
  arbitrary = elements [Buy, Sell]

data OrdType
  = Market
  | Limit
  deriving (Eq, Show)
instance Arbitrary OrdType where
  arbitrary = elements [Market, Limit]

data FirmQuote
  = FQ_Yes
  | FQ_No
  deriving (Eq, Show)
instance Arbitrary FirmQuote where
  arbitrary = elements [FQ_Yes, FQ_No]

data Priority
  = Lost
  | Retained
  deriving (Eq, Show)
instance Arbitrary Priority where
  arbitrary = elements [Lost, Retained]

class AddOrder where
    orderId  :: Word32
    side     :: Types.Side
    quantity :: Types.Qty

