-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.BookTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Data.TheBook.Book'.
-----------------------------------------------------------------------------
module Data.TheBook.BookTest (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.List
import Data.Ord
import Data.TheBook.Book as Book
import Data.TheBook.Types as Types

tests :: TestTree
tests = testGroup "Data.TheBook.TheBookTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "fromList == stable sort by price . toList" isCorrectlySorted
  ]

isCorrectlySorted :: [(Types.Price, Types.Qty)]
                  -> Bool
isCorrectlySorted entries = let book = Book.fromList entries :: Book.Book Book.Buy
                            in Book.toList book == sortBy (comparing fst) entries

