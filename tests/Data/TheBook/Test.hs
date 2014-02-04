-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Untitled
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- 
-----------------------------------------------------------------------------
module Data.TheBook.Test where

import qualified Data.TheBook.TheBookTest as TheBookTest
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
      TheBookTest.tests
    ]
