-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2013, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Electronic order book in Haskell.
-----------------------------------------------------------------------------
module Main (
   module Types
 , main
 ) where

import           Data.TheBook.Book  as Book
import           Data.TheBook.Types as Types
import           System.Exit

-- 51 = [100, 100, 50]
-- 50 = [50]
-- 45 = [5]

main :: IO ()
main = do
    putStrLn "Hello World"
    putStrLn "Wassup"
    putStrLn "Hello travis"
    print $ Book.toList (book6 :: Book Buy)
    exitSuccess
    where book1 = insert 51 100 empty :: Book.Book Book.Buy
          book2 = insert 51 100 book1
          book3 = insert 51 50  book2
          book4 = insert 50 10  book3
          book5 = insert 45 5   book4
          book6 = insert 50.0 5 book5



