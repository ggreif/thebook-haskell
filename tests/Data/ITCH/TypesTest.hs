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
import qualified Data.ByteString.Char8 as B8
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.List
import Data.Ord
import qualified Data.ITCH.Types as ITCH
import Debug.Trace (trace, traceShow)
import Data.Time.Calendar (Day, toGregorian, fromGregorian)

tests :: TestTree
tests = testGroup "Data.TheBook.ITCHTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "encode (decode date) == d" encodeDecodeDate
  ]

encodeDecodeDate :: ITCH.Date -> Bool
encodeDecodeDate d@(ITCH.Date d1) =
    let encoded = B.encode d
        decoded = B.decode encoded
    in (d == decoded) && L.length encoded == 8
