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
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.ITCHTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "msg == msg" eqTest
  , QC.testProperty "decode (encode msg) == msg" serialiseDeserialise
  , QC.testProperty "block of messages should deserialise cleanly" serialiseDeserialiseBlock
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
        deserialised = map (\(_, _, msg) -> msg) (rights (singleFold serialised))
        singleFold b
          | L.null b  = []
          | otherwise = case (Get.runGetOrFail (B.get :: Get.Get ITCH.ITCHMessage) b) of
                        l@(Left (remaining, _, _)) -> [l] ++ (singleFold remaining)
                        r@(Right (remaining, _, newMsg)) -> [r] ++ (singleFold remaining)
