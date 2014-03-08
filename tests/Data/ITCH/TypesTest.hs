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
  []

