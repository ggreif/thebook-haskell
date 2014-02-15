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
import Control.Monad
import qualified Data.TheBook.Book as Book
import System.Exit

main :: IO ()
main = do
    putStrLn "Hello World"
    let book  = Book.insert 50.0 100 Book.empty
        book1 = Book.insert 50.0 123 book
        book2 = Book.insert 51.0 99  book1
    forM_ (map Book.showBook [book, book1, book2]) putStrLn
    exitSuccess



