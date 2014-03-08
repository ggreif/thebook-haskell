{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ITCH
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
--
-- The messages here can be used standalone, or serialised directly into ITCH format from
-- corresponding 'Data.TheBook.MarketData' messages, in order to improve on memory
-- allocation.
-----------------------------------------------------------------------------
module Data.ITCH.Types (
    -- | Message header
    MessageHeader, messageLength, messageType, messageHeaderLength

    -- | Types
  , Alpha, BitField, Date, Time, UInt8, UInt16, UInt32, UInt64, Byte, Price

  ) where

import Data.Bits (bit, testBit, (.|.))
import Data.Word
import Data.Decimal (Decimal, realFracToDecimal)
import qualified Data.ByteString.Char8 as B8
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord16le, getWord32le, getWord64le)
import Data.Binary.Put (putWord16le, putWord32le, putWord64le)
import Control.Applicative ((<$>), (<*>), (*>), pure)
import Control.Monad (fail)
import qualified Data.TheBook.MarketData as Types
import Foreign.Storable (Storable, sizeOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Calendar (Day, fromGregorian)

-- Look at: http://hackage.haskell.org/package/binary-0.5.0.2/docs/Data-Binary-Get.html
-- http://www.haskell.org/haskellwiki/Dealing_with_binary_data

-- * Data types
-- | Data Type | Length   | Description
-- | -----------------------------------
-- | Alpha     | Variable | These fields use standard ASCII character bytes.
-- |           |          | They are left justified and padded on the right with spaces.
-- | -----------------------------------
newtype Alpha = Alpha B8.ByteString
  deriving (Eq, Show)
instance Arbitrary Alpha where
  arbitrary = Alpha . B8.pack <$> arbitrary

-- | Bit Field | 1        | A single byte used to hold up to eight 1-bit flags.
--             |          | Each bit will represent a Boolean flag.
-- |           |          | The 0 bit is the lowest significant bit and the 7 bit is the highest significant bit.
type BitField = Word8

-- | Byte      | 1        | A single byte used to hold one ASCII character.
type Byte = Word8

-- | Date      | 8        | Date specified in the YYYYMMDD format using ASCII characters.
newtype Date = Date Day
  deriving (Eq, Show)
instance Arbitrary Date where
  arbitrary = Date <$> (fromGregorian <$> arbitrary <*> arbitrary <*> arbitrary)

-- | Time      | 8        | Time specified in the HH:MM:SS format using ASCII characters.
newtype Time = Time DiffTime
  deriving (Eq, Show)
instance Arbitrary Time where
  arbitrary = Time . secondsToDiffTime <$> arbitrary

-- | Price     | 8        | Signed Little-Endian encoded eight byte integer field with eight implied decimal places.
newtype Price = Price Decimal
  deriving (Eq, Show)
instance Arbitrary Price where
  arbitrary = Price <$> (realFracToDecimal <$> arbitrary <*> (arbitrary :: Gen Double))

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
class Binary a => MessageHeader a where
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
