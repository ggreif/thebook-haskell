{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE TypeFamilies               #-}
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
-----------------------------------------------------------------------------
module Data.ITCH.Types (
    -- | Message header
    MessageHeader, messageLength, messageType, messageHeaderLength

    -- | Types
  , Alpha, BitField, Date(..), Time, UInt8, UInt16, UInt32, UInt64, Byte, Price

    -- | Utilities
  , getMessageType, putMessageType
  ) where

import           Control.Applicative       (pure, (*>), (<$>), (<*>))
import           Control.Monad             (fail)
import           Data.Binary               (Binary, get, put)
import           Data.Binary.Get           (Get, getByteString, getWord16le,
                                            getWord32le, getWord64le)
import           Data.Binary.Put           (Put, putByteString, putWord16le,
                                            putWord32le, putWord64le)
import           Data.Bits                 (Bits, bit, shiftR, testBit, (.|.))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8 (ByteString, pack)
import qualified Data.ByteString.Internal  as BSI
import qualified Data.ByteString.Unsafe    as BSU
import           Data.Decimal              (Decimal, realFracToDecimal)
import qualified Data.TheBook.MarketData   as Types
import           Data.Time.Calendar        (Day, fromGregorian, toGregorian)
import           Data.Time.LocalTime       (TimeOfDay, dayFractionToTimeOfDay)
import           Data.Time.Format          (formatTime)
import           Data.Word
import           Debug.Trace               (trace, traceShow)
import           Foreign.Ptr               (Ptr, plusPtr)
import           Foreign.Storable          (Storable, peek, poke, sizeOf)
import           System.Locale             (defaultTimeLocale)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen       (Gen, elements, suchThat)

-- * Data types
-- | Data Type | Length   | Description
-- | -----------------------------------
-- | Alpha     | Variable | These fields use standard ASCII character bytes.
-- |           |          | They are left justified and padded on the right with spaces.
-- | -----------------------------------
newtype Alpha = Alpha BS8.ByteString
  deriving (Eq, Show)
instance Arbitrary Alpha where
  arbitrary = Alpha . BS8.pack <$> arbitrary
instance Binary Alpha where
  get = undefined
  put (Alpha a) = undefined

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
  arbitrary = Date <$> (fromGregorian <$> suchThat arbitrary (> 0) <*> suchThat arbitrary (> 0) <*> suchThat arbitrary (> 0))
instance Binary Date where
  get = Date <$> (fromGregorian <$> readNum 4 <*> readNum 2 <*> readNum 2)
  put (Date d) = putByteString . BS8.pack $ formatTime defaultTimeLocale "%0Y%m%d" d

-- | Time      | 8        | Time specified in the HH:MM:SS format using ASCII characters.
newtype Time = Time TimeOfDay
  deriving (Eq, Show)
instance Arbitrary Time where
  arbitrary = Time . dayFractionToTimeOfDay <$> suchThat arbitrary (\d -> d >= 0 || d <= 1)
instance Binary Time where
  get = undefined -- Time <$> (fromGregorian <$> readNum 4 <*> readNum 2 <*> readNum 2)
  put (Time t) = undefined

-- | Price     | 8        | Signed Little-Endian encoded eight byte integer field with eight implied decimal places.
newtype Price = Price Decimal
  deriving (Eq, Show)
instance Arbitrary Price where
  arbitrary = Price <$> (realFracToDecimal <$> arbitrary <*> (arbitrary :: Gen Double))
instance Binary Price where
  get = undefined
  put (Price p) = undefined

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

-- | Simplifies getting the msg type in generated code.
getMessageType :: Get Byte
getMessageType = get

putMessageType :: Byte -> Put
putMessageType = put

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

-- | Private definitions.

-- | Tries to read a number from a bytestring of the given length
-- and calls 'fail' if it is not possible.
readNum :: (Integral a) => Int -> Get a
readNum i = do
  num <- getByteString i
  let maybeNum = readDecimal num
  case maybeNum of
    Just (a, b) | BS.null b -> pure a
    _                       -> fail "Couldn't parse number"

-- | Writes the number.
writeNum :: (Integral a) => a -> Put
writeNum i = case packDecimal i of
              Just a -> putByteString a
              _      -> fail "Cannot encode decimal"

-- Copied from 'Data.ByteString.Lex.Numerical',
-- because I can't get it to install on Haskell Center.

-- | Read an unsigned\/non-negative integral value in ASCII decimal
-- format. Returns @Nothing@ if there is no integer at the beginning
-- of the string, otherwise returns @Just@ the integer read and the
-- remainder of the string.
--
-- If you are extremely concerned with performance, then it is more
-- performant to use this function at @Int@ or @Word@ and then to
-- call 'fromIntegral' to perform the conversion at the end. However,
-- doing this will make your code succeptible to overflow bugs if
-- the target type is larger than @Int@.
readDecimal :: Integral a => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readDecimal ::
    ByteString -> Maybe (Int,     ByteString),
    ByteString -> Maybe (Integer, ByteString),
    ByteString -> Maybe (Word,    ByteString),
    ByteString -> Maybe (Word8,   ByteString),
    ByteString -> Maybe (Word16,  ByteString),
    ByteString -> Maybe (Word32,  ByteString),
    ByteString -> Maybe (Word64,  ByteString) #-}
readDecimal = start
    where
    isDecimal :: Word8 -> Bool
    {-# INLINE isDecimal #-}
    isDecimal w = 0x39 >= w && w >= 0x30

    toDigit :: Integral a => Word8 -> a
    {-# INLINE toDigit #-}
    toDigit w = fromIntegral (w - 0x30)

    addDigit :: Int -> Word8 -> Int
    {-# INLINE addDigit #-}
    addDigit n w = n * 10 + toDigit w

    -- TODO: should we explicitly drop all leading zeros before we jump into the unrolled loop?
    start :: Integral a => ByteString -> Maybe (a, ByteString)
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> Just $ loop0 (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> Nothing

    loop0 :: Integral a => a -> ByteString -> (a, ByteString)
    loop0 m xs
        | m `seq` xs `seq` False = undefined
        | BS.null xs = (m, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop1 m (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> (m, xs)

    loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8
        :: Integral a => a -> Int -> ByteString -> (a, ByteString)
    loop1 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop2 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*10 + fromIntegral n, xs)
    loop2 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop3 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*100 + fromIntegral n, xs)
    loop3 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*1000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop4 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*1000 + fromIntegral n, xs)
    loop4 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop5 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*10000 + fromIntegral n, xs)
    loop5 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop6 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*100000 + fromIntegral n, xs)
    loop6 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*1000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop7 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*1000000 + fromIntegral n, xs)
    loop7 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop8 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> (m*10000000 + fromIntegral n, xs)
    loop8 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop0
                    (m*1000000000 + fromIntegral (addDigit n w))
                    (BSU.unsafeTail xs)
              | otherwise   -> (m*100000000 + fromIntegral n, xs)

----------------------------------------------------------------
-- | Convert a non-negative integer into an (unsigned) ASCII decimal
-- string. Returns @Nothing@ on negative inputs.
packDecimal :: (Integral a) => a -> Maybe ByteString
{-# INLINE packDecimal #-}
packDecimal n
    | n < 0     = Nothing
    | otherwise = Just (unsafePackDecimal n)


-- This implementation is modified from:
-- <http://www.serpentine.com/blog/2013/03/20/whats-good-for-c-is-good-for-haskell/>
--
-- | Convert a non-negative integer into an (unsigned) ASCII decimal
-- string. This function is unsafe to use on negative inputs.
unsafePackDecimal :: (Integral a) => a -> ByteString
{-# SPECIALIZE unsafePackDecimal ::
    Int     -> ByteString,
    Integer -> ByteString,
    Word    -> ByteString,
    Word8   -> ByteString,
    Word16  -> ByteString,
    Word32  -> ByteString,
    Word64  -> ByteString #-}
unsafePackDecimal n0 =
    let size = numDecimalDigits n0
    in  BSI.unsafeCreate size $ \p0 -> loop n0 (p0 `plusPtr` (size - 1))
    where
    getDigit = BSU.unsafeIndex packDecimal_digits

    loop n p
        | n `seq` p `seq` False = undefined -- for strictness analysis
        | n >= 100  = do
            let (q,r) = n `quotRem` 100
            write2 r p
            loop   q (p `plusPtr` negate 2)
        | n >= 10   = write2 n p
        | otherwise = poke p (0x30 + fromIntegral n)

    write2 i0 p
        | i0 `seq` p `seq` False = undefined -- for strictness analysis
        | otherwise = do
            let i = fromIntegral i0; j = i + i
            poke p                      (getDigit $! j + 1)
            poke (p `plusPtr` negate 1) (getDigit j)

packDecimal_digits :: ByteString
{-# NOINLINE packDecimal_digits #-}
packDecimal_digits = BS8.pack
    "0001020304050607080910111213141516171819\
    \2021222324252627282930313233343536373839\
    \4041424344454647484950515253545556575859\
    \6061626364656667686970717273747576777879\
    \8081828384858687888990919293949596979899"
    -- BUG: syntax highlighting fail: ->

----------------------------------------------------------------
----------------------------------------------------------------
----- Integral logarithms

-- TODO: cf. integer-gmp:GHC.Integer.Logarithms made available in version 0.3.0.0 (ships with GHC 7.2.1).
-- <http://haskell.org/ghc/docs/7.2.1/html/libraries/integer-gmp-0.3.0.0/GHC-Integer-Logarithms.html>


-- This implementation is derived from
-- <http://www.haskell.org/pipermail/haskell-cafe/2009-August/065854.html>
-- modified to use 'quot' instead of 'div', to ensure strictness,
-- and using more guard notation (but this last one's compiled
-- away). See @./test/bench/BenchNumDigits.hs@ for other implementation
-- choices.
--
-- | @numDigits b n@ computes the number of base-@b@ digits required
-- to represent the number @n@. N.B., this implementation is unsafe
-- and will throw errors if the base is @(<= 1)@, or if the number
-- is negative. If the base happens to be a power of 2, then see
-- 'numTwoPowerDigits' for a more efficient implementation.
--
-- We must be careful about the input types here. When using small
-- unsigned types or very large values, the repeated squaring can
-- overflow causing the function to loop. (E.g., the fourth squaring
-- of 10 overflows 32-bits (==1874919424) which is greater than the
-- third squaring. For 64-bit, the 5th squaring overflows, but it's
-- negative so will be caught.) Forcing the type to Integer ensures
-- correct behavior, but makes it substantially slower.

numDigits :: Integer -> Integer -> Int
{-# INLINE numDigits #-}
numDigits b0 n0
    | b0 <= 1   = error (_numDigits ++ _nonpositiveBase)
    | n0 <  0   = error (_numDigits ++ _negativeNumber)
    -- BUG: need to check n0 to be sure we won't overflow Int
    | otherwise = 1 + fst (ilog b0 n0)
    where
    ilog b n
        | n < b     = (0, n)
        | r < b     = ((,) $! 2*e) r
        | otherwise = ((,) $! 2*e+1) $! (r `quot` b)
        where
        (e, r) = ilog (b*b) n


-- | Compute the number of base-@2^p@ digits required to represent a
-- number @n@. N.B., this implementation is unsafe and will throw
-- errors if the base power is non-positive, or if the number is
-- negative. For bases which are not a power of 2, see 'numDigits'
-- for a more general implementation.
numTwoPowerDigits :: (Integral a, Bits a) => Int -> a -> Int
{-# INLINE numTwoPowerDigits #-}
numTwoPowerDigits p n0
    | p  <= 0   = error (_numTwoPowerDigits ++ _nonpositiveBase)
    | n0 <  0   = error (_numTwoPowerDigits ++ _negativeNumber)
    | n0 == 0   = 1
    -- BUG: need to check n0 to be sure we won't overflow Int
    | otherwise = go 0 n0
    where
    go d n
        | d `seq` n `seq` False = undefined
        | n > 0     = go (d+1) (n `shiftR` p)
        | otherwise = d

-- This implementation is from:
-- <http://www.serpentine.com/blog/2013/03/20/whats-good-for-c-is-good-for-haskell/>
--
-- | Compute the number of base-@10@ digits required to represent
-- a number @n@. N.B., this implementation is unsafe and will throw
-- errors if the number is negative.
numDecimalDigits :: (Integral a) => a -> Int
{-# INLINE numDecimalDigits #-}
numDecimalDigits n0
    | n0 < 0     = error (_numDecimalDigits ++ _negativeNumber)
    -- Unfortunately this causes significant (1.2x) slowdown since
    -- GHC can't see it will always fail for types other than Integer...
    | n0 > limit = numDigits 10 (toInteger n0)
    | otherwise  = go 1 (fromIntegral n0 :: Word64)
    where
    limit = fromIntegral (maxBound :: Word64)

    fin n bound = if n >= bound then 1 else 0
    go k n
        | k `seq` False = undefined -- For strictness analysis
        | n < 10        = k
        | n < 100       = k + 1
        | n < 1000      = k + 2
        | n < 1000000000000 =
            k + if n < 100000000
                then if n < 1000000
                    then if n < 10000
                        then 3
                        else 4 + fin n 100000
                    else 6 + fin n 10000000
                else if n < 10000000000
                    then 8 + fin n 1000000000
                    else 10 + fin n 100000000000
        | otherwise = go (k + 12) (n `quot` 1000000000000)

_numDigits :: String
_numDigits = "numDigits"
{-# NOINLINE _numDigits #-}

_numTwoPowerDigits :: String
_numTwoPowerDigits = "numTwoPowerDigits"
{-# NOINLINE _numTwoPowerDigits #-}

_numDecimalDigits :: String
_numDecimalDigits = "numDecimalDigits"
{-# NOINLINE _numDecimalDigits #-}

_nonpositiveBase :: String
_nonpositiveBase = ": base must be greater than one"
{-# NOINLINE _nonpositiveBase #-}

_negativeNumber :: String
_negativeNumber = ": number must be non-negative"
{-# NOINLINE _negativeNumber #-}
