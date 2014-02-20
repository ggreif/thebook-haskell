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

-- Look at: http://hackage.haskell.org/package/binary-0.5.0.2/docs/Data-Binary-Get.html
-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data

-- Data types:
-- Data Type | Length   | Description
-- -----------------------------------
-- Alpha     | Variable | These fields use standard ASCII character bytes.
--           |          | They are left justified and padded on the right with spaces.
-- -----------------------------------
-- Bit Field | 1        | A single byte used to hold up to eight 1-bit flags.
--           |          | Each bit will represent a Boolean flag. The 0 bit is the lowest significant bit and the 7 bit is the highest significant bit.
-- Byte      | 1        | A single byte used to hold one ASCII character.
-- Date      | 8        | Date specified in the YYYYMMDD format using ASCII characters.
-- Time      | 8        | Time specified in the HH:MM:SS format using ASCII characters.
-- Price     | 8        | Signed Little-Endian encoded eight byte integer field with eight implied decimal places.
-- UInt8     | 1        | 8 bit unsigned integer.
-- UInt16    | 2        | Little-Endian encoded 16 bit unsigned integer.
-- UInt32    | 4        | Little-Endian encoded 32 bit unsigned integer.
-- UInt64    | 8        | Little-Endian encoded 64 bit unsigned integer.

-- Unit header:
-- Field             | Offset | Length   | Type   | Description
-- ----------------------------------------------------------
-- Length            | 0      | 2        | UInt16 | Length of the message block including the header and all payload messages.
-- Message Count     | 2      | 1        | UInt8  | Number of payload messages that will follow the header.
-- Market Data Group | 3      | 1        |  Byte  | Identity of the market data group the payload messages relate to.
--                   |        |          |        | This field is not validated for client initiated messages.
-- Sequence Number   | 4      | 4        | UInt32 | Sequence number of the first payload message.
-- Payload           | 8      | Variable | -      | One or more payload messages.


-- 4.9.5 Add Order
--
-- Field         | Offset | Length | Type      | Description
-- ---------------------------------------------------------
-- Length        | 0      | 1      | UInt8     | Length of message including this field.
-- Message Type  | 1      | 1      | Byte      | Hex=0x41; Meaning=Add Order
-- Nanosecond    | 2      | 4      | UInt32    | Nanoseconds since last Time message, accurate to the nearest microsecond.
-- Order ID      | 6      | 8      | UInt64    | Unique identifier of the order.
-- Side          | 14     | 1      | Byte      | Value Meaning: B=Buy Order; S=Sell Order
-- Quantity      | 15     | 4      | UInt32    | Displayed quantity of the order.
-- Instrument ID | 19     | 4      | UInt32    | Instrument identifier
-- Reserved      | 23     | 1      | Byte      | Reserved field
-- Reserved      | 24     | 1      | Byte      | Reserved field
-- Price         | 25     | 8      | Price     | Limit price of the order.
-- Flags         | 33     | 1      | Bit Field | Bit | Name         | Meaning
--                                             | 4   | Market Order | No=0
--                                             |     |              | Yes=1
class AddOrder where
    orderId  :: Word32
    side     :: Types.Side
    quantity :: Types.Qty
