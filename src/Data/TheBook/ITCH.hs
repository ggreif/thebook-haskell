{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- This module contains ITCH messages and their serialisers,
-- as per the specs at
-- <http://www.londonstockexchange.com/products-and-services/millennium-exchange/technicalinformation/technicalinformation.htm>,
-- specifically:
-- <http://www.londonstockexchange.com/products-and-services/millennium-exchange/millennium-exchange-migration/mit303.pdf>.
-----------------------------------------------------------------------------
module Data.TheBook.ITCH (
    AddOrder
  , MessageHeader, messageLength, messageType
  , messageHeaderLength
  ) where

import Data.Bits (bit)
import Data.Word
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord16le, getWord32le, getWord64le)
import Data.Binary.Put (putWord16le, putWord32le, putWord64le)
import Control.Applicative ((<$>), (<*>), (*>), pure)
import Control.Monad (fail)
import qualified Data.TheBook.MarketData as Types
import Foreign.Storable (Storable, sizeOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)

-- Look at: http://hackage.haskell.org/package/binary-0.5.0.2/docs/Data-Binary-Get.html
-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data

-- * Data types
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
newtype UInt16 = UInt16 Word16
  deriving (Eq, Show, Storable, Arbitrary)
instance Binary UInt16 where
  get = UInt16 <$> getWord16le
  put (UInt16 uint16) = putWord16le uint16

-- UInt32    | 4        | Little-Endian encoded 32 bit unsigned integer.
newtype UInt32 = UInt32 Word32
  deriving (Eq, Show, Storable, Arbitrary)
instance Binary UInt32 where
  get = UInt32 <$> getWord32le
  put (UInt32 uint32) = putWord32le uint32

-- UInt64    | 8        | Little-Endian encoded 64 bit unsigned integer.
newtype UInt64 = UInt64 Word64
  deriving (Eq, Show, Storable, Arbitrary)
instance Binary UInt64 where
  get = UInt64 <$> getWord64le
  put (UInt64 uint64) = putWord64le uint64

-- * Reusable types and values
-- Some of the types are just newtypes around existing types
-- in 'Data.TheBook.MarketData', in order to provide appropriate
-- 'Data.Binary' instances.

newtype Side = Side Types.Side
  deriving (Eq, Show, Arbitrary)
instance Binary Side where
  get = do
    side <- get
    if | side == _B -> return $ Side Types.Buy
       | side == _S -> return $ Side Types.Sell
       | otherwise  -> fail "Unknown side"
  put (Side side) = put $ if side == Types.Buy then _B else _S

newtype OrdType = OrdType Types.OrdType
  deriving (Eq, Show, Arbitrary)
instance Binary OrdType where
  get = do
    orderType <- get
    if | orderType == _Market_No  -> return $ OrdType Types.Limit
       | orderType == _Market_Yes -> return $ OrdType Types.Market
       | otherwise                -> fail "Unknown orderType"
  put (OrdType orderType) = put $ if | orderType == Types.Limit  -> _Market_No
                                     | orderType == Types.Market -> _Market_Yes
                                     | otherwise                 -> error "This is impossible"

-- | Buy order
_B :: Byte
_B = 0x42

-- | Sell order
_S :: Byte
_S = 0x53

_Market_No :: Word8
_Market_No =  0x0

_Market_Yes :: Word8
_Market_Yes = bit 4

-- | Zero byte
_0 :: Word8
_0 = 0x0

-- Unit header:
-- Field             | Offset | Length   | Type   | Description
-- ----------------------------------------------------------
-- Length            | 0      | 2        | UInt16 | Length of the message block including the header and all payload messages.
-- Message Count     | 2      | 1        | UInt8  | Number of payload messages that will follow the header.
-- Market Data Group | 3      | 1        | Byte   | Identity of the market data group the payload messages relate to.
--                   |        |          |        | This field is not validated for client initiated messages.
-- Sequence Number   | 4      | 4        | UInt32 | Sequence number of the first payload message.
-- Payload           | 8      | Variable | -      | One or more payload messages.

-- * Message header
-- Defines the message type and length.

-- | Message header
class MessageHeader a where
  messageLength :: a -> UInt8
  messageType   :: a -> Byte

-- Field             | Offset | Length   | Type   | Description
-- ----------------------------------------------------------
-- Length            | 0      | 1        | UInt8  | Length of message including this field.
-- Message Type      | 1      | 1        | Byte   | Message type
-- Nanosecond        | 2      | 4        | UInt32 | Nanoseconds since last Time message,
--                                                | accurate to the nearest microsecond.
messageHeaderLength :: Int
messageHeaderLength
  = sizeOf (0 :: UInt8)  + -- Length
    sizeOf (0 :: Byte)   + -- Message Type
    sizeOf (UInt32 0)      -- Nanosecond

-- * 4.9. Application Messages
-- $applicationMessages

-- ** 4.9.5 Add Order (Hex=0x41 - A)
--

-- | Field         | Offset | Length | Type      | Description
--  ---------------------------------------------------------
data AddOrder = AddOrder {

  -- Order ID      | 6      | 8      | UInt64    | Unique identifier of the order.
    orderId           :: {-# UNPACK #-} !UInt64

  -- Side          | 14     | 1      | Byte      | Value Meaning: B=Buy Order; S=Sell Order
  , side              :: {-# UNPACK #-} !Side

  -- Quantity      | 15     | 4      | UInt32    | Displayed quantity of the order.
  , quantity          :: {-# UNPACK #-} !UInt32

  -- Instrument ID | 19     | 4      | UInt32    | Instrument identifier
  , instrumentId      :: {-# UNPACK #-} !UInt32

  -- Reserved      | 23     | 1      | Byte      | Reserved field
  , addOrderReserved1 :: {-# UNPACK #-} !Byte

  -- Reserved      | 24     | 1      | Byte      | Reserved field
  , addOrderReserved2 :: {-# UNPACK #-} !Byte

  -- Price         | 25     | 8      | Price     | Limit price of the order.
  , price             :: {-# UNPACK #-} !Price

  -- Flags         | 33     | 1      | Bit Field | Bit | Name         | Meaning
  --                                             | 4   | Market Order | No=0
  --                                             |     |              | Yes=1
  , flags             :: {-# UNPACK #-} !OrdType
} deriving (Show, Eq)

instance MessageHeader AddOrder where
    messageLength a
      = fromIntegral $ sizeOf (UInt64 1) + -- OrderId
        sizeOf (0 :: Byte)               + -- Side
        sizeOf (UInt32 0)                + -- Quantity
        sizeOf (UInt32 0)                + -- Instrument ID
        sizeOf (0 :: Byte)               + -- Reserved
        sizeOf (0 :: Byte)               + -- Reserved
        sizeOf (UInt64 0)                + -- Price
        sizeOf (0 :: Byte)                 -- Flags
    messageType a = 0x41 -- A

instance Binary AddOrder where
  get = AddOrder <$> get
                 <*> get
                 <*> get
                 <*> get
                 <*> get
                 <*> get
                 <*> get
                 <*> get
  put AddOrder {..} =
    put orderId           *>
    put side              *>
    put quantity          *>
    put instrumentId      *>
    put addOrderReserved1 *>
    put addOrderReserved2 *>
    put price             *>
    put flags

instance Arbitrary AddOrder where
  arbitrary
    = AddOrder <$> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary

-- * 4.9.7. Order Deleted (Hex: 0x44)
--
-- | Field         | Offset | Length | Type      | Description
--  ---------------------------------------------------------



