-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Order
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- 
-----------------------------------------------------------------------------
module Data.TheBook.Order where

import qualified Data.TheBook.Types as Types
import Data.Text (Text)

-- | Represents a limit order.
data LimitOrder = LimitOrder {
    id    :: Text        -- ^ Assigned unique id of the order
  , side  :: Types.Side  -- ^ Side of the order
  , price :: Types.Price -- ^ Limit price
  , qty   :: Types.Qty   -- ^ Quantity
}