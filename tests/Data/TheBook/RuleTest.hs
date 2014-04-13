-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.RuleTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Data.TheBook.Rule'.
-----------------------------------------------------------------------------
module Data.TheBook.RuleTest (tests) where

import           Data.List
import           Data.Ord
import           Data.TheBook.Rule     as Book
import           Data.TheBook.Types    as Types
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.RuleTest" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Bla" True
  ]
