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

import qualified Data.ITCH.ITCH51Test   as ITCH51Test
import qualified Data.ITCH.TypesTest    as ITCHTypesTest
import qualified Data.TheBook.BookTest  as BookTest
import qualified Data.TheBook.MonadTest as Monad
import qualified Data.TheBook.RuleTest  as RuleTest
import qualified Data.TheBook.TypesTest as TypesTest
import qualified Test.Tasty             as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests" [
    BookTest.tests
  , ITCHTypesTest.tests
  , ITCH51Test.tests
  , RuleTest.tests
  , TypesTest.tests
  , Monad.tests
  ]
