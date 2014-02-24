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
  , QC.testProperty "length (AddOrder)   == 28" addOrderMessageLengthProp
  , QC.testProperty "decode (encode (AddOrder)) == AddOrder" addOrderMessageBinaryProp
  , QC.testProperty "length (AddOrder) == length (encode (AddOrder))" addOrderMessageBinaryLengthProp
  ]

addOrderMessageLengthProp :: ITCH.AddOrder -> Bool
addOrderMessageLengthProp addOrder = ITCH.messageLength addOrder == 28

addOrderMessageBinaryProp :: ITCH.AddOrder -> Bool
addOrderMessageBinaryProp addOrder = let encoded        = B.encode addOrder
                                         decoded        = B.decode encoded :: ITCH.AddOrder
                                         lengthEncoded  = L.length encoded
                                         lengthExpected = fromIntegral $ ITCH.messageLength addOrder
                                     in addOrder == decoded && lengthEncoded == lengthExpected

addOrderMessageBinaryLengthProp :: ITCH.AddOrder -> Bool
addOrderMessageBinaryLengthProp addOrder = let encoded        = B.encode addOrder
                                               lengthEncoded  = L.length encoded
                                               lengthExpected = fromIntegral $ ITCH.messageLength addOrder
                                           in lengthEncoded == lengthExpected