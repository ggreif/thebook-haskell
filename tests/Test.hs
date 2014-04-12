-----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- All tests.
-----------------------------------------------------------------------------
module Main where

import qualified Data.ITCH.ITCH51Test  as ITCH51Test
import qualified Data.ITCH.TypesTest   as TypesTest
import qualified Data.TheBook.BookTest as BookTest
import           Test.Tasty            as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests" [
    BookTest.tests
  , TypesTest.tests
  , ITCH51Test.tests
  ]