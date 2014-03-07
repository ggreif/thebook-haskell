-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ITCHTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests 'Data.ITCH'.
-----------------------------------------------------------------------------
module Data.ITCH.TypesTest (tests) where

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as L
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.List
import Data.Ord
import qualified Data.ITCH.Types as ITCH
import Debug.Trace (traceShow)

tests :: TestTree
tests = testGroup "Data.TheBook.ITCHTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "messageHeaderLength == 6" (ITCH.messageHeaderLength == 6)
  -- AddOrder
  , QC.testProperty "length (AddOrder) == 28" addOrderMessageLengthProp
  , QC.testProperty "decode (encode (AddOrder)) == AddOrder" (messageBinaryProp :: ITCH.AddOrder -> Bool)
  , QC.testProperty "length (AddOrder) == length (encode (AddOrder))" (messageBinaryProp :: ITCH.AddOrder -> Bool)
  -- OrderDeleted
  , QC.testProperty "length (OrderDeleted) == 13" orderDeletedMessageLengthProp
  , QC.testProperty "decode (encode (OrderDeleted)) == OrderDeleted" (messageBinaryProp :: ITCH.OrderDeleted -> Bool)
  , QC.testProperty "length (DeleteOrder) == length (encode (OrderDeleted))" (messageBinaryProp :: ITCH.OrderDeleted -> Bool)
  -- OrderModified
  , QC.testProperty "length (OrderModified) == 21" orderModifiedMessageLengthProp
  , QC.testProperty "decode (encode (OrderModified)) == OrderModified" (messageBinaryProp :: ITCH.OrderModified -> Bool)
  , QC.testProperty "length (DeleteOrder) == length (encode (OrderModified))" (messageBinaryProp :: ITCH.OrderModified -> Bool)
  -- OrderBookClear
  , QC.testProperty "length (OrderBookClear) == 7" bookClearMessageLengthProp
  , QC.testProperty "decode (encode (OrderBookClear)) == OrderBookClear" (messageBinaryProp :: ITCH.OrderBookClear -> Bool)
  , QC.testProperty "length (OrderBookClear) == length (encode (OrderBookClear))" (messageBinaryProp :: ITCH.OrderBookClear -> Bool)
  -- OrderExecuted
  , QC.testProperty "length (OrderExecuted) == 7" orderExecutedMessageLengthProp
  , QC.testProperty "decode (encode (OrderExecuted)) == OrderExecuted" (messageBinaryProp :: ITCH.OrderExecuted -> Bool)
  , QC.testProperty "length (OrderExecuted) == length (encode (OrderExecuted))" (messageBinaryProp :: ITCH.OrderExecuted -> Bool)
    -- OrderExecutedWithPriceSize
  , QC.testProperty "length (OrderExecutedWithPriceSize) == 33" orderExecutedWithPriceSizeMessageLengthProp
  , QC.testProperty "decode (encode (OrderExecutedWithPriceSize)) == OrderExecutedWithPriceSize" (messageBinaryProp :: ITCH.OrderExecutedWithPriceSize -> Bool)
  , QC.testProperty "length (OrderExecutedWithPriceSize) == length (encode (OrderExecutedWithPriceSize))" (messageBinaryProp :: ITCH.OrderExecutedWithPriceSize -> Bool)
  ]

addOrderMessageLengthProp :: ITCH.AddOrder -> Bool
addOrderMessageLengthProp addOrder = ITCH.messageLength addOrder == 28

orderDeletedMessageLengthProp :: ITCH.OrderDeleted -> Bool
orderDeletedMessageLengthProp orderDeleted = ITCH.messageLength orderDeleted == 13

orderModifiedMessageLengthProp :: ITCH.OrderModified -> Bool
orderModifiedMessageLengthProp orderDeleted = ITCH.messageLength orderDeleted == 21

bookClearMessageLengthProp :: ITCH.OrderBookClear -> Bool
bookClearMessageLengthProp orderDeleted = ITCH.messageLength orderDeleted == 7

orderExecutedMessageLengthProp :: ITCH.OrderExecuted -> Bool
orderExecutedMessageLengthProp orderDeleted = ITCH.messageLength orderDeleted == 20

orderExecutedWithPriceSizeMessageLengthProp :: ITCH.OrderExecutedWithPriceSize -> Bool
orderExecutedWithPriceSizeMessageLengthProp orderDeleted = ITCH.messageLength orderDeleted == 33

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
