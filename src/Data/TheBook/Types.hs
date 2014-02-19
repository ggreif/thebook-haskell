-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Types
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Main types used throughout the code.
-----------------------------------------------------------------------------
module Data.TheBook.Types where

import qualified Data.Time.Clock as Clock
import Data.Text (Text)

-- | Type of a price.
type Price = Double

-- | Type of a quantity.
type Qty = Int

-- | Tradable instrument.
type Instrument = Text

-- | UTC Time.
type Time = Clock.UTCTime

-- | Indicates the side of an order.
data Side = Buy  -- ^ Indicates a buy order.
          | Sell -- ^ Indicates a sell order.

-- | Allowed order types
data OrderType
  = Market -- | 1
  | Limit  -- | 2
