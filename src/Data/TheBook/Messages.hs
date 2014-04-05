-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Messages
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- 
-----------------------------------------------------------------------------
module Data.TheBook.Messages where

import Data.Text (Text)
import qualified Data.TheBook.Types as Types

-- | The new order message type is used by institutions wishing
-- to electronically submit securities and forex orders to a
-- broker for execution.
--
-- The New Order message type may also be used by
-- institutions or retail intermediaries wishing to electronically
-- submit Collective Investment Vehicle (CIV) orders to a broker
-- or fund manager for execution.
data NewOrderSingle = NewOrderSingle {

    -- | Unique identifier of the order as assigned
    -- by institution or by the intermediary (CIV term,
    -- not a hub/service bureau) with closest association
    -- with the investor.
    clOrdId :: !Text

    -- | Insert here the set of "Instrument" (symbology)
    -- fields defined in "Common Components of Application Messages".
  , instrument :: !Types.Instrument

    -- | Side of the order.
  , side :: !Types.Side

    -- | Time this order request was initiated/released by the trader,
    -- trading system, or intermediary.
  , transactTime :: !Types.Time

  , orderType :: !Types.OrderType
}