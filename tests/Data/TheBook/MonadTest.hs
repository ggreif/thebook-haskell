-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.MonadTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Data.TheBook.Monad'.
-----------------------------------------------------------------------------
module Data.TheBook.MonadTest (tests) where

import           Data.TheBook.Monad
import           Data.TheBook.Types    as Types
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.MonadTest" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "dummy" True
  ]

