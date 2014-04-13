{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Types
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Main types used throughout the code.
-----------------------------------------------------------------------------
module Data.TheBook.Types (
    Price, TickSize, Qty
  , Instrument, Exchange, Currency
  , Time, Side(..), OrderType(..)
  , DictionaryRecord, TickRule(..)
  , WithDictionary, WithSession, session, SessionID
) where

import           Control.Lens              (makeLenses)
import           Control.Monad             (MonadPlus, mplus, mzero)
import           Data.ByteString           (ByteString)
import           Data.Function             (on)
import           Data.List                 (find, sortBy)
import           Data.Text                 (Text)
import qualified Data.Time.Clock           as Clock
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen       (elements)

-- | Type of a price.
type Price = Double

-- | Size of a tick.
type TickSize = Double

-- | Type of a quantity.
type Qty = Int

-- | Tradable instrument.
type Instrument = Text

-- | Name of an exchange.
type Exchange = Text

-- | Name of a currency.
type Currency = Text

-- | UTC Time.
type Time = Clock.UTCTime

-- | Indicates the side of an order.
data Side
  = Buy  -- ^ Indicates a buy order.
  | Sell -- ^ Indicates a sell order.
  deriving (Eq, Show)
instance Arbitrary Side where
  arbitrary = elements [Buy, Sell]

-- | Allowed order types
data OrderType
  = Market -- | 1
  | Limit  -- | 2
  deriving (Eq, Show)
instance Arbitrary OrderType where
  arbitrary = elements [Market, Limit]

-- | I'll have to think if this is providing me with the
-- flexibility I want of not having to tie in to a specific
-- reader/state representation.
-- It might be a bit clunky though.
class WithSession a where
  session :: a -> SessionID

type SessionID = ByteString

-- | Tick rule specifies price increments at given price range.
data TickRule = TickRule {

    -- | Size of a tick.
    _tickSize  :: TickSize

    -- | At which price does this rule apply.
  , _tickPrice :: Price
} deriving (Show)
makeLenses ''TickRule

-- | Information about an instrument that can be traded.
data DictionaryRecord = DictionaryRecord {

    -- | Name of the instrument.
    _dInstrument :: Instrument

    -- | Exchange of the instrument.
  , _dExchange   :: Exchange

    -- | Currency of the instrument.
  , _dCurrency   :: Currency

    -- | Tick rules of the instrument.
  , _dTickRules  :: [TickRule]
} deriving (Show)
makeLenses ''DictionaryRecord

class WithDictionary a where
  dict :: a -> DictionaryRecord

-- | Creates a new 'Dictionary'.
dictionary :: Instrument
           -> Exchange
           -> Currency
           -> [TickRule]
           -> DictionaryRecord
dictionary ins exchange currency rules
  = DictionaryRecord {
        _dInstrument = ins
      , _dExchange   = exchange
      , _dTickRules  = sortBy (compare `on` _tickPrice) rules
    }

-- | Tries to get 'TickRule' for this price.
tickRuleForPrice :: MonadPlus m
                 => DictionaryRecord
                 -> Price
                 -> m TickRule
tickRuleForPrice d p = case find ((==) p . _tickPrice) (_dTickRules d) of
  Just t  -> return t
  Nothing -> mzero


-- MonadError e m => MonadError e (ParsecT s u m)
-- MonadReader r m => MonadReader r (ParsecT s u m)
-- MonadState s m => MonadState s (ParsecT s' u m)
-- MonadTrans (ParsecT s u)
-- Monad (ParsecT s u m)
-- Functor (ParsecT s u m)
-- MonadPlus (ParsecT s u m)
-- Applicative (ParsecT s u m)
-- Alternative (ParsecT s u m)
-- MonadIO m => MonadIO (ParsecT s u m)
-- MonadCont m => MonadCont (ParsecT s u m)
