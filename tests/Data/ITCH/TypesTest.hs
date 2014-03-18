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
module Data.ITCH.TypesTest (tests, trickyDate) where

import           Control.Applicative
import           Control.Arrow
import qualified Data.Binary           as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import           Data.Int
import qualified Data.ITCH.Types       as ITCH
import           Data.Time.Calendar    (Day, fromGregorian, toGregorian)
import           Debug.Trace           (trace, traceShow)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import Test.QuickCheck.Gen

tests :: TestTree
tests = testGroup "Data.TheBook.ITCHTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ testProperty "encode (decode Date) == Date" (encodeDecode 8 :: ITCH.Date -> Bool)
  , testProperty "encode (decode tricky Date) == tricky Date" encodeDecodeTrickyDate
  , testProperty "encode (decode Time) == Time" (encodeDecode 8 :: ITCH.Time -> Bool)
  , testProperty "encode (decode Price) == Price" (encodeDecode 8 :: ITCH.Price -> Bool)
  ]

encodeDecode :: (Eq a, B.Binary a) => Int64 -> a -> Bool
encodeDecode i d =
    let encoded = B.encode d
        decoded = B.decode encoded
    in (d == decoded) && L.length encoded == i

encodeDecodeTrickyDate :: Property
encodeDecodeTrickyDate = forAll trickyDate (\d -> traceShow d ((d == (B.decode . B.encode $ d)) && L.length (B.encode d) == 8))

trickyDate :: Gen ITCH.Date
trickyDate = ITCH.Date <$> (fromGregorian
            <$> suchThat arbitrary trickyYear
            <*> suchThat arbitrary trickyMonth
            <*> suchThat arbitrary trickyDay)
  where trickyYear x  = x > 0 && x < 1000
        trickyMonth x = x > 0 && x < 10
        trickyDay x   = x > 0 && x < 10