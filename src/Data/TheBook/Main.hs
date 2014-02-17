-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.TheBook
-- Copyright   :  (c) 2013, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Exchange simulator in Haskell.
-----------------------------------------------------------------------------
module Data.TheBook.Main (
   module Data.TheBook.Types
 , module Data.TheBook.Book
 , main
 ) where

import Data.TheBook.Types
import Data.TheBook.Book
import System.Exit

main :: IO ()
main = do
    putStrLn "Hello World"
    exitSuccess



