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

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as L
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.List
import Data.Ord
import qualified Data.ITCH.ITCH51 as ITCH
import Debug.Trace (trace, traceShow)

tests :: TestTree
tests = testGroup "Data.TheBook.ITCHTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "msg == msg" eqTest
  ]

eqTest :: ITCH.ITCHMessage -> Bool
eqTest msg = msg == msg