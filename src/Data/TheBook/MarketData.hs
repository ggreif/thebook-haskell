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
module Data.TheBook.MarketData where

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

class AddOrder where
    orderId  :: Word32
    side     :: Types.Side
    quantity :: Types.Qty

