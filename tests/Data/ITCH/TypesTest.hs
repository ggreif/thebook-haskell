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

import qualified Data.Binary           as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import           Data.Int
import qualified Data.ITCH.Types       as ITCH
import           Data.Time.Calendar    (Day, fromGregorian, toGregorian)
import           Debug.Trace           (trace, traceShow)
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.ITCHTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "encode (decode Date) == Date" (encodeDecode 8 :: ITCH.Date -> Bool)
  , QC.testProperty "encode (decode Time) == Time" (encodeDecode 8 :: ITCH.Time -> Bool)
  , QC.testProperty "encode (decode Price) == Price" (encodeDecode 8 :: ITCH.Price -> Bool)
  --, QC.testProperty "encode (decode Alpha) == ALpha" encodeDecodeAlpha
  ]

encodeDecode :: (Eq a, B.Binary a) => Int64 -> a -> Bool
encodeDecode i d =
    let encoded = B.encode d
        decoded = B.decode encoded
    in (d == decoded) && L.length encoded == i