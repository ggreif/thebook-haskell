-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ITCH.ITCH51Test
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests 'Data.ITCH.ITCH51'.
-----------------------------------------------------------------------------
module Data.ITCH.ITCH51Test (tests) where

import qualified Data.Binary           as B
import qualified Data.Binary.Get       as Get
import qualified Data.ByteString.Lazy  as L
import Data.Either
import qualified Data.ITCH.ITCH51      as ITCH
import qualified Data.ITCH.Types       as Types
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.ITCHTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "msg == msg" eqTest
  , QC.testProperty "decode (encode msg) == msg" serialiseDeserialise
  , QC.testProperty "block of messages should deserialise cleanly" serialiseDeserialiseBlock
  , QC.testProperty "block of right padded should deserialise cleanly" serialiseDeserialiseBlockDifferentLengths
  , QC.testProperty "block of messages with unit header should deserialise cleanly" serialiseDeserialiseBlockWithUnitHeader
  ]

eqTest :: ITCH.ITCHMessage -> Bool
eqTest msg = msg == msg

serialiseDeserialise :: ITCH.ITCHMessage -> Bool
serialiseDeserialise msg = let serialised   = B.encode msg
                               deserialised = B.decode serialised :: ITCH.ITCHMessage
                           in deserialised == msg

serialiseDeserialiseBlock :: [ITCH.ITCHMessage] -> Bool
serialiseDeserialiseBlock msgs = deserialised == msgs
  where serialised = L.concat $ map B.encode msgs
        deserialised = deserialiseBlock serialised

serialiseDeserialiseBlockDifferentLengths :: [ITCH.ITCHMessage] -> Bool
serialiseDeserialiseBlockDifferentLengths msgs = deserialised == msgs
  where serialised = L.concat $ map (padMessage 100 . B.encode) msgs
        deserialised = deserialiseBlock serialised

deserialiseBlock :: L.ByteString -> [ITCH.ITCHMessage]
deserialiseBlock bs = map (\(_, _, msg) -> msg) (rights (singleFold bs))
  where singleFold b
          | L.null b  = []
          | otherwise = case (Get.runGetOrFail (B.get :: Get.Get ITCH.ITCHMessage) b) of
                        l@(Left (remaining, _, _)) -> [l] ++ (singleFold remaining)
                        r@(Right (remaining, _, newMsg)) -> [r] ++ (singleFold remaining)

serialiseDeserialiseBlockWithUnitHeader :: Types.Byte -> Types.UInt32 -> [ITCH.ITCHMessage] -> Bool
serialiseDeserialiseBlockWithUnitHeader marketDataGroup seqNo msg =
  let serialised   = Types.writeMessages marketDataGroup seqNo msg
      unitHeader   = (Get.runGet Types.readMessages serialised) :: Types.UnitHeader ITCH.ITCHMessage
  in Types._unitHeaderPayload unitHeader == msg &&
     Types._unitHeaderMessageCount unitHeader == (fromIntegral $ length msg) &&
     Types._unitHeaderMarketDataGroup unitHeader == marketDataGroup &&
     Types._unitHeaderSequenceNumber unitHeader == seqNo

-- | Pads the message on the right with additional bytes
--  and updates the message length.
padMessage :: Int -> L.ByteString -> L.ByteString
padMessage padding bs =
  let curLength = Get.runGet Types.getMessageLength bs
      newLength = curLength + (fromIntegral padding)
      paddingBs = L.replicate (fromIntegral padding) 0x00
  in (newLength `L.cons` (L.tail bs)) `L.append` paddingBs