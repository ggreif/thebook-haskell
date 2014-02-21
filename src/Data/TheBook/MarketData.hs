{-# LANGUAGE NullaryTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.MarketData
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- 
-----------------------------------------------------------------------------
module Data.TheBook.MarketData where

import Data.Word (Word32)
import qualified Data.TheBook.Types as Types

class AddOrder where
    orderId  :: Word32
    side     :: Types.Side
    quantity :: Types.Qty
