{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.ITCH
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- 
-----------------------------------------------------------------------------
module Data.TheBook.ITCH where

import Data.Bits (bit)
import Data.Word
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord16le, getWord32le, getWord64le)
import Data.Binary.Put (putWord16le, putWord32le, putWord64le)
import Control.Applicative ((<$>))
import qualified Data.TheBook.Types as Types

-- Look at: http://hackage.haskell.org/package/binary-0.5.0.2/docs/Data-Binary-Get.html
-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data

-- Data types:
-- | Data Type | Length   | Description
-- | -----------------------------------
-- | Alpha     | Variable | These fields use standard ASCII character bytes.
-- |           |          | They are left justified and padded on the right with spaces.
-- | -----------------------------------
-- | Bit Field | 1        | A single byte used to hold up to eight 1-bit flags.
--             |          | Each bit will represent a Boolean flag.
-- |           |          | The 0 bit is the lowest significant bit and the 7 bit is the highest significant bit.
type BitField = Word8

-- | Byte      | 1        | A single byte used to hold one ASCII character.

type Byte = Word8

-- | Date      | 8        | Date specified in the YYYYMMDD format using ASCII characters.
-- | Time      | 8        | Time specified in the HH:MM:SS format using ASCII characters.

-- | Price     | 8        | Signed Little-Endian encoded eight byte integer field with eight implied decimal places.
type Price = UInt64

-- | UInt8     | 1        | 8 bit unsigned integer.
type UInt8 = Word8

-- | UInt16    | 2        | Little-Endian encoded 16 bit unsigned integer.
newtype UInt16 = UInt16 { unUInt16 :: Word16 }
instance Binary UInt16 where
  get = UInt16 <$> getWord16le
  put (UInt16 uint16) = putWord16le uint16

-- UInt32    | 4        | Little-Endian encoded 32 bit unsigned integer.
newtype UInt32 = UInt32 { unUInt32 :: Word32 }
instance Binary UInt32 where
  get = UInt32 <$> getWord32le
  put (UInt32 uint32) = putWord32le uint32

-- UInt64    | 8        | Little-Endian encoded 64 bit unsigned integer.
newtype UInt64 = UInt64 { unUInt64 :: Word64 }
instance Binary UInt64 where
  get = UInt64 <$> getWord64le
  put (UInt64 uint64) = putWord64le uint64

-- Unit header:
-- Field             | Offset | Length   | Type   | Description
-- ----------------------------------------------------------
-- Length            | 0      | 2        | UInt16 | Length of the message block including the header and all payload messages.
-- Message Count     | 2      | 1        | UInt8  | Number of payload messages that will follow the header.
-- Market Data Group | 3      | 1        |  Byte  | Identity of the market data group the payload messages relate to.
--                   |        |          |        | This field is not validated for client initiated messages.
-- Sequence Number   | 4      | 4        | UInt32 | Sequence number of the first payload message.
-- Payload           | 8      | Variable | -      | One or more payload messages.

-- | Buy order
_B :: Word8
_B = 0x42

-- | Sell order
_S :: Word8
_S = 0x53

_Market_No :: Word8
_Market_No =  0x0

_Market_Yes :: Word8
_Market_Yes = bit 4

-- | Zero byte
_0 :: Word8
_0 = 0x0

-- 4.9.5 Add Order
--
-- Field         | Offset | Length | Type      | Description
-- ---------------------------------------------------------
-- | Length        | 0      | 1      | UInt8     | Length of message including this field.
-- | Message Type  | 1      | 1      | Byte      | Hex=0x41; Meaning=Add Order
-- | Nanosecond    | 2      | 4      | UInt32    | Nanoseconds since last Time message, accurate to the nearest microsecond.
data IAddOrder = IAddOrder {

  -- Order ID      | 6      | 8      | UInt64    | Unique identifier of the order.
    orderId    :: UInt64

  -- Side          | 14     | 1      | Byte      | Value Meaning: B=Buy Order; S=Sell Order
  , side       :: Types.Side

  -- Quantity      | 15     | 4      | UInt32    | Displayed quantity of the order.
  , quantity   :: UInt32

  -- Instrument ID | 19     | 4      | UInt32    | Instrument identifier
  , instrumentId :: UInt32

  -- Reserved      | 23     | 1      | Byte      | Reserved field
  -- Ignored

  -- Reserved      | 24     | 1      | Byte      | Reserved field
  -- Ignored

  -- Price         | 25     | 8      | Price     | Limit price of the order.
  , price :: Price

  -- Flags         | 33     | 1      | Bit Field | Bit | Name         | Meaning
  --                                             | 4   | Market Order | No=0
  --                                             |     |              | Yes=1
  , flags :: Types.OrderType
}

instance Binary IAddOrder where
  get = do
    orderId <- get
    side    <- get
    let sideS = if | side == _B -> Types.Buy
                   | side == _S -> Types.Sell
                   | otherwise  -> error "Unknown side"
    quantity <- get
    instrumentId <- get
    addOrderR1 <- get :: Get Byte
    addOrderR2 <- get :: Get Byte
    price <- get
    flags <- get
    let flagsF = if | flags == _Market_Yes -> Types.Market
                    | flags == _Market_No  -> Types.Limit
                    | otherwise            -> error "Unknown flags"
    return IAddOrder {
               orderId = orderId
             , side = sideS
             , quantity = quantity
             , instrumentId = instrumentId
             , price = price
             , flags = flagsF
           }
  put IAddOrder {..} = do
    put orderId
    if side == Types.Buy
    then put _B
    else put _S
    put quantity
    put price
    if flags == Types.Market
    then put _Market_Yes
    else put _Market_No



