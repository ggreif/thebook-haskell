{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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

data Buy
data Sell

newtype SortablePrice a = SortablePrice Types.Price
instance Eq (SortablePrice Buy) where
      p1 == p2 = p1 == p2
instance Eq (SortablePrice Sell) where
      p1 == p2 = p2 == p2
instance Ord (SortablePrice Buy) where
      compare = compare
instance Ord (SortablePrice Sell) where
      compare p1 p2 = compare p2 p1


-- | Limit order book.
type Book a = Map (SortablePrice a) (Seq.Seq Entry)

-- | Empty limit order book
empty :: Ord (SortablePrice a) => Book a
empty = Map.empty

insert :: Ord (SortablePrice a)
       => Types.Price
       -> Types.Qty
       -> Book a
       -> Book a
insert price qty = Map.alter (Just . Maybe.maybe newLevel (Seq.|> newEntry )) (SortablePrice price)
  where newEntry = Entry { price = price, qty = qty }
        newLevel :: Seq.Seq Entry
        newLevel = Seq.singleton newEntry

-- | Creates a 'Book' from a list of ('Types.Price', 'Types.Qty') pairs.
fromList :: Ord (SortablePrice a) => [(Types.Price, Types.Qty)]
         -> Book a
fromList = List.foldr (Tuple.uncurry insert) empty

-- | Creates correctly sorted list of ('Types.Price', 'Types.qty') pairs from this 'Book'.
toList :: Ord (SortablePrice a) => Book a
       -> [(Types.Price, Types.Qty)]
toList book = let entries = Map.elems book
                  seqs    = Fold.concatMap Fold.toList entries
              in map (price &&& qty) seqs
