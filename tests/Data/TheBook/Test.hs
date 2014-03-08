-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Untitled
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- All tests.
-----------------------------------------------------------------------------
module Main where

import qualified Data.TheBook.BookTest as BookTest
import qualified Data.ITCH.TypesTest as TypesTest
import qualified Data.ITCH.ITCH51Test as ITCH51Test
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    BookTest.tests
  , TypesTest.tests
  , ITCH51Test.tests
  ]
