-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.ITCHTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests 'Data.TheBook.ITCH'.
-----------------------------------------------------------------------------
module Data.TheBook.ITCHTest (tests) where

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as L
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.List
import Data.Ord
import qualified Data.TheBook.ITCH as ITCH
import Debug.Trace (traceShow)

tests :: TestTree
tests = testGroup "Data.TheBook.ITCHTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "messageHeaderLength == 6" (ITCH.messageHeaderLength == 6)
  -- AddOrder
  , QC.testProperty "length (AddOrder) == 28" addOrderMessageLengthProp
  , QC.testProperty "decode (encode (AddOrder)) == AddOrder" (messageBinaryProp :: ITCH.AddOrder -> Bool)
  , QC.testProperty "length (AddOrder) == length (encode (AddOrder))" (messageBinaryProp :: ITCH.AddOrder -> Bool)
  -- DeleteOrder
  , QC.testProperty "length (OrderDeleted) == 13" orderDeletedMessageLengthProp
  , QC.testProperty "decode (encode (OrderDeleted)) == OrderDeleted" (messageBinaryProp :: ITCH.OrderDeleted -> Bool)
  , QC.testProperty "length (DeleteOrder) == length (encode (OrderDeleted))" (messageBinaryProp :: ITCH.OrderDeleted -> Bool)
  ]

addOrderMessageLengthProp :: ITCH.AddOrder -> Bool
addOrderMessageLengthProp addOrder = ITCH.messageLength addOrder == 28

orderDeletedMessageLengthProp :: ITCH.OrderDeleted -> Bool
orderDeletedMessageLengthProp orderDeleted = ITCH.messageLength orderDeleted == 13

messageBinaryProp :: (ITCH.MessageHeader a, B.Binary a, Eq a) => a -> Bool
messageBinaryProp m = let encoded        = B.encode m
                          decoded        = B.decode encoded
                          lengthEncoded  = L.length encoded
                          lengthExpected = fromIntegral $ ITCH.messageLength m
                      in m == decoded && lengthEncoded == lengthExpected

messageBinaryLengthProp :: (ITCH.MessageHeader a, B.Binary a) => a -> Bool
messageBinaryLengthProp m = let encoded        = B.encode m
                                lengthEncoded  = L.length encoded
                                lengthExpected = fromIntegral $ ITCH.messageLength m
                            in lengthEncoded == lengthExpected