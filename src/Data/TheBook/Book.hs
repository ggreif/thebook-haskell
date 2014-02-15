-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Book
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Limit order book implementation that maintains price and time priority.
-- The time is inferred, not actually specified. This means that u
-----------------------------------------------------------------------------
module Data.TheBook.Book (
      Book
    , Entry
    , empty
    , insert
    , showBook
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe
import qualified Data.TheBook.Types as Types

-- | Limit price passive order sitting on the 'Book'.
data Entry = Entry {
      price :: Types.Price
    , qty   :: Types.Qty
} deriving Show

-- | Limit order book.
newtype Book = Book { unBook :: Map.Map Types.Price (Seq.Seq Entry) }
  deriving Show

-- | Empty limit order book
empty :: Book
empty = Book { unBook = Map.empty }

insert :: Types.Price
       -> Types.Qty
       -> Book
       -> Book
insert price qty book = Book { unBook = Map.alter (Just . Maybe.maybe newLevel (Seq.|> newEntry )) price (unBook book) }
  where newEntry = Entry { price = price, qty = qty }
        newLevel :: Seq.Seq Entry
        newLevel = Seq.singleton newEntry

showBook :: Book -> String
showBook = Map.showTree . unBook
