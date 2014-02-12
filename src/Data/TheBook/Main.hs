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
   --module Data.TheBook.Types
 --, module Data.TheBook.Book
 ) where

import Data.TheBook.Types
import Data.TheBook.Book
import Control.Monad
import qualified Data.TheBook.Book as Book

main :: IO ()
main = do
    putStrLn "Hello World"
    let book  = Book.insert 50.0 100 Book.empty
        book1 = Book.insert 50.0 123 book1
        book2 = Book.insert 51.0 99  book2
    forM_ (map show [book, book1, book2]) putStrLn
    putStrLn "Hello World"



