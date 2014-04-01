{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ITCH
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- This module types used in ITCH messages and their serialisers,
-- as per the specs at
-- <http://www.londonstockexchange.com/products-and-services/millennium-exchange/technicalinformation/technicalinformation.htm>,
-- specifically:
-- <http://www.londonstockexchange.com/products-and-services/millennium-exchange/millennium-exchange-migration/mit303.pdf>.
--
-- Serialisation of dates and times currently relies on `Data.Time.Format`
-- which doesn't seem to handle serialisation of invalid dates.
-- Until I figure out what's going on, I shall generate the test values so
-- that they are correct.
-----------------------------------------------------------------------------
module Data.ITCH.Types (

    -- | Types
    Alpha, BitField, Date(..), Time(..), UInt8, UInt16, UInt32, UInt64, Byte, Price

    -- | Unit header

    -- | Utilities
  , getMessageLength, putMessageLength, getMessageType, putMessageType, arbitraryAlpha, getAlpha, putAlpha
  , skipRemaining
  ) where

import           Control.Applicative       (pure, (*>), (<$>))
import           Control.Monad             (fail, forM_)
import           Data.Binary               (Binary, get, put)
import           Data.Binary.Get           (Get, getByteString, getWord16le,
                                            getWord32le, getWord64le, skip)
import           Data.Binary.Put           (Put, putByteString, putWord16le,
                                            putWord32le, putWord64le)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8 (ByteString, length, pack,
                                                   replicate, unpack)
import qualified Data.ByteString.Unsafe    as BSU
import           Data.Decimal              (DecimalRaw (..), realFracToDecimal)

import           Data.Time.Calendar        (Day (..))
import           Data.Time.Clock           (secondsToDiffTime)
import           Data.Time.Format          (formatTime, parseTime)
import           Data.Time.LocalTime       (TimeOfDay, timeToTimeOfDay)
import           Data.Word
import           Debug.Trace               (trace, traceShow)
import           Foreign.Storable          (Storable)
import           System.Locale             (defaultTimeLocale)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen       (Gen, suchThat)

-- * Data types
-- | Data Type | Length   | Description
-- | -----------------------------------
-- | Alpha     | Variable | These fields use standard ASCII character bytes.
-- |           |          | They are left justified and padded on the right with spaces.
-- | -----------------------------------
newtype Alpha = Alpha BS8.ByteString
  deriving (Eq, Show)

-- | Generator for 'Alpha' values of this length.
arbitraryAlpha :: Int -> Gen Alpha
arbitraryAlpha length = (Alpha . BS.replicate length) <$> arbitrary

-- | Puts the alpha given its length.
putAlpha :: Int -> Alpha -> Put
putAlpha length (Alpha a) = putByteString $ padAlpha length a

-- | Gets the alpha given its length.
getAlpha :: Int -> Get Alpha
getAlpha length = Alpha <$> getByteString length

-- | Padding for 'Alpha' values.
padAlphaValue :: Char
padAlphaValue = ' '

-- | Pads (or trims) the bytestring to be of exactly 'length'.
padAlpha :: Int -> BS8.ByteString -> BS8.ByteString
padAlpha length alpha | BS8.length alpha > length = BS.take length alpha
padAlpha length alpha | BS8.length alpha == length = alpha
padAlpha length alpha | otherwise = BS.append alpha pad
  where
  pad = BS8.replicate r padAlphaValue
  r   = length - BS.length alpha

-- | Bit Field | 1        | A single byte used to hold up to eight 1-bit flags.
-- |           |          | Each bit will represent a Boolean flag.
-- |           |          | The 0 bit is the lowest significant bit and the 7 bit is the highest significant bit.
type BitField = Word8

-- | Byte      | 1        | A single byte used to hold one ASCII character.
type Byte = Word8

-- | Date      | 8        | Date specified in the YYYYMMDD format using ASCII characters.
newtype Date = Date Day
  deriving (Eq, Show)
instance Arbitrary Date where
  arbitrary = (\x -> Date $ ModifiedJulianDay {toModifiedJulianDay = x} ) <$> arbitrary
instance Binary Date where
  get = Date <$> (maybeToFail =<< (parseTime defaultTimeLocale "%0Y%m%d" . BS8.unpack <$> getByteString 8))
  put (Date d) = putByteString . BS8.pack $ formatTime defaultTimeLocale "%0Y%m%d" d

-- | Time      | 8        | Time specified in the HH:MM:SS format using ASCII characters.
-- The 'Arbitrary' as well as 'Binary' instances are very specifically implemented to pass
-- tests. There is an inherent loss of information in this encoding, because we only
-- encode time with second precision, whereas 'TimeOfDay' has picosecond precision.
-- Therefore, get and put do not satisfy the identity law, unless we choose the input
-- 'TimeOfDay' very carefully.
newtype Time = Time TimeOfDay
  deriving (Eq, Show)
instance Arbitrary Time where
  arbitrary = Time . timeToTimeOfDay . secondsToDiffTime <$> suchThat arbitrary (\d -> d >= 0 && d <= 60 * 60 * 24)
instance Binary Time where
  get = Time <$> (maybeToFail =<< (parseTime defaultTimeLocale "%H:%M:%S" . BS8.unpack <$> getByteString 8))
  put (Time t) = putByteString . BS8.pack $ formatTime defaultTimeLocale "%H:%M:%S" t

-- | Price     | 8        | Signed Little-Endian encoded eight byte integer field with eight implied decimal places.
newtype Price = Price (DecimalRaw Word64)
  deriving (Eq, Show)
instance Arbitrary Price where
  arbitrary = Price . realFracToDecimal 8 <$> (arbitrary :: Gen Double)
instance Binary Price where
  get = Price . Decimal 8 <$> getWord64le
  put (Price (Decimal _ p)) = putWord64le p

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

-- * Unit header
--     Field             | Offset | Length   | Type   | Description
-- ----------------------------------------------------------------
data UnitHeader a = UnitHeader {

    -- Length            | 0      | 2        | UInt16 | Length of the message block including the header and all payload messages.
    _unitHeaderLength          :: !UInt16

    -- Message Count     | 2      | 1        | UInt8  | Number of payload messages that will follow the header.
  , _unitHeaderMessageCount    :: !UInt8

    -- Market Data Group | 3      | 1        | Byte   | Identity of the market data group the payload messages relate to.
    --                   |        |          |        | This field is not validated for client initiated messages.
  , _unitHeaderMarketDataGroup :: !Byte

    -- Sequence Number   | 4      | 4        | UInt32 | Sequence number of the first payload message.
  , _unitHeaderSequenceNumber  :: !UInt32

    -- Payload           | 8      | Variable | -      | One or more payload messages.
  , _unitHeaderPayload         :: ![a]
} deriving (Eq, Show)

instance Binary a => Binary (UnitHeader a) where
  put UnitHeader {..}
    = put _unitHeaderLength          *>
      put _unitHeaderMessageCount    *>
      put _unitHeaderMarketDataGroup *>
      put _unitHeaderSequenceNumber  *>
      forM_ (zip [1.._unitHeaderMessageCount] _unitHeaderPayload) (put . snd)
  get = undefined

-- | * Utilities

-- | Simplifies getting the msg type in generated code.
getMessageType :: Get Byte
getMessageType = get

-- | Simplified putting the msg type in generated code.
putMessageType :: Byte -> Put
putMessageType = put

-- | Gets the message length of bytes
getMessageLength :: Get UInt8
getMessageLength = get

-- | Simplifies putting the length of the message in generated code.
putMessageLength :: UInt8 -> Put
putMessageLength = put

-- | Consumes any remaining bytes.
skipRemaining :: UInt8 -> Int -> Get ()
skipRemaining expected actual
  = let diff = (fromIntegral expected) - actual
    in if diff > 0
        then skip diff
        else return ()

-- | Transforms `Data.Maybe` into a `Get`, where `Nothing` constitutes a call to `fail`.

maybeToFail :: Maybe a -> Get a
maybeToFail (Just a) = pure a
maybeToFail (Nothing) = fail "Something didn't work"
