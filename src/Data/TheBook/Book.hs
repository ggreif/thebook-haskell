{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Book
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Limit order book implementation that maintains price and time priority.
-- The time is inferred, not actually specified.
-----------------------------------------------------------------------------
module Data.TheBook.Book (
      Book
    , Entry
    , Buy
    , Sell
    , price
    , qty
    , empty
    , insert
    , fromList
    , toList
    , showBook
 ) where

import qualified Data.Foldable as Fold
import qualified Data.Tuple as Tuple
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe
import qualified Data.TheBook.Types as Types
import Control.Arrow

-- | Limit price passive order sitting on the 'Book'.
data Entry = Entry {
      price :: Types.Price
    , qty   :: Types.Qty
} deriving Show

class Side a where
    priceS :: Types.Price

newtype Buy = Buy Types.Price
newtype Sell = Sell Types.Price

instance Eq Buy where
    (==) = (==)
instance Eq Sell where
    (==) = (==)
instance Ord Buy where
    compare = compare
instance Ord Sell where
    p1 `compare` p2 = p2 `compare` p1

-- | Limit order book.
type Book a = Map a (Seq.Seq Entry)

-- | Empty limit order book
empty :: Book a
empty = Map.empty

insert :: Types.Price
       -> Types.Qty
       -> Book a
       -> Book a
insert price qty = Map.alter (Just . Maybe.maybe newLevel (Seq.|> newEntry )) price
  where newEntry = Entry { price = price, qty = qty }
        newLevel :: Seq.Seq Entry
        newLevel = Seq.singleton newEntry

-- | Creates a 'Book' from a list of ('Types.Price', 'Types.Qty') pairs.
fromList :: [(Types.Price, Types.Qty)]
         -> Book a
fromList = List.foldr (Tuple.uncurry insert) empty

-- | Creates correctly sorted list of ('Types.Price', 'Types.qty') pairs from this 'Book'.
toList :: Book a
       -> [(Types.Price, Types.Qty)]
toList book = let entries = Map.elems book
                  seqs    = Fold.concatMap Fold.toList entries
              in map (price &&& qty) seqs

showBook :: Show a => Book a -> String
showBook = Map.showTree
