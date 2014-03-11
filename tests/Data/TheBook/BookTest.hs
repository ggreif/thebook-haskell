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

import           Data.List
import           Data.Ord
import           Data.TheBook.Book     as Book
import           Data.TheBook.Types    as Types
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.TheBookTest" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "fromList buys == reverse stable sort by price . toList" isCorrectlySortedBuy
  , QC.testProperty "fromList sells == stable sort by price . toList" isCorrectlySortedSell
  ]

isCorrectlySortedSell :: [(Types.Price, Types.Qty)]
                      -> Bool
isCorrectlySortedSell entries = let book = Book.fromList entries :: Book.Book Book.Sell
                                in Book.toList book == sortBy (comparing fst) entries
isCorrectlySortedBuy :: [(Types.Price, Types.Qty)]
                     -> Bool
isCorrectlySortedBuy entries = let book = Book.fromList entries :: Book.Book Book.Buy
                               in Book.toList book == sortBy (flip $ comparing fst) entries
