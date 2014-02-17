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
   module Types
 , module Book
 , main
 ) where

import Data.TheBook.Types as Types
import Data.TheBook.Book as Book
import System.Exit

main :: IO ()
main = do
    putStrLn "Hello World"
    putStrLn "Wassup"
    putStrLn $ showBook book2
    exitSuccess
    where book1 = insert 50 100 empty
          book2 = insert 51 100 book1



