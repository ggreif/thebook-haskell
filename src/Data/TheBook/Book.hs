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
    , Side
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

class (Ord a) => Side a where
    priceS :: Types.Price -> a

newtype Buy = Buy Types.Price
  deriving (Show)
newtype Sell = Sell Types.Price
  deriving (Show)

instance Eq Buy where
    (Buy p1) == (Buy p2) = p1 == p2
instance Eq Sell where
    (Sell p1) == (Sell p2) = p2 == p1
instance Ord Sell where
    (Sell p1) `compare` (Sell p2) = p1 `compare` p2
instance Ord Buy where
    (Buy p1) `compare` (Buy p2) = p2 `compare` p1
instance Side Buy where
    priceS = Buy
instance Side Sell where
    priceS = Sell

-- | Limit order book.
newtype Book a = Book (Map a (Seq.Seq Entry))
  deriving (Show)

-- | Empty limit order book
empty :: Book a
empty = Book Map.empty

insert :: Side a =>
          Types.Price
       -> Types.Qty
       -> Book a
       -> Book a
insert price qty (Book book) = Book $ Map.alter (Just . Maybe.maybe newLevel (Seq.|> newEntry )) (priceS price) book
  where newEntry = Entry { price = price, qty = qty }
        newLevel :: Seq.Seq Entry
        newLevel = Seq.singleton newEntry

-- | Creates a 'Book' from a list of ('Types.Price', 'Types.Qty') pairs.
fromList :: Side a
         => [(Types.Price, Types.Qty)]
         -> Book a
fromList = List.foldr (Tuple.uncurry insert) empty

-- | Creates correctly sorted list of ('Types.Price', 'Types.qty') pairs from this 'Book'.
toList :: Side a
       => Book a
       -> [(Types.Price, Types.Qty)]
toList (Book book) = let entries = Map.elems book
                         seqs    = Fold.concatMap Fold.toList entries
                     in map (price &&& qty) seqs

showBook :: Show a => Book a -> String
showBook (Book book) = Map.showTree book
