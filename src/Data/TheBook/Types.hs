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
  , Instrument, Market, Currency
  , Time
  , Dictionary, TickRule(..), WithDictionary, dictL
  , WithSession, sessionL, SessionID

  -- * Enums
  , Side(..)
  , OrderType(..)

  -- * Outgoing messages
  , CancelReject
) where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens              (Lens', makeLenses)
import           Control.Monad             (MonadPlus, mplus, mzero)
import           Data.ByteString           (ByteString)
import           Data.Function             (on)
import           Data.List                 (find, sortBy)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Time.Clock           as Clock
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen       (Gen, elements)

-- | Type of id of 'Order'.
type OrderID = ByteString

-- | Type of a price.
type Price = Double

-- | Size of a tick.
type TickSize = Double

-- | Type of a quantity.
type Qty = Int

-- | Tradable instrument.
data Instrument = Instrument {
    _iMarket :: !Market
  , _iSymbol :: !Text
} deriving (Eq, Show)
instance Arbitrary Instrument where
  arbitrary = Instrument <$> arbitraryText <*> arbitraryText

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

-- | Name of an exchange.
type Market = Text

-- | Name of a currency.
type Currency = Text

-- | UTC Time.
type Time = Clock.UTCTime

-- | Used to identify the sender of the message.
type SessionID = ByteString

-- | Extracts 'SessionID' from the state.
class WithSession a where
  sessionL :: Lens' a SessionID

-- | Tick rule specifies price increments at given price range.
data TickRule = TickRule {

    -- | Size of a tick.
    _tickSize  :: !TickSize

    -- | At which price does this rule apply.
  , _tickPrice :: !Price
} deriving (Eq, Show)
makeLenses ''TickRule

-- | Information about an instrument that can be traded.
data Dictionary = Dictionary {

    -- | Name of the instrument.
    _dInstrument :: !Instrument

    -- | Currency of the instrument.
  , _dCurrency   :: Currency

    -- | Tick rules of the instrument.
  , _dTickRules  :: [TickRule]
} deriving (Eq, Show)
makeLenses ''Dictionary

-- | Extracts the 'Dictionary' from
class WithDictionary a where
  dictL :: Lens' a Dictionary

-- | Creates a new 'Dictionary'.
dictionary :: Instrument
           -> Currency
           -> [TickRule]
           -> Dictionary
dictionary instrument' currency' rules'
  = Dictionary {
        _dInstrument = instrument'
      , _dCurrency = currency'
      , _dTickRules  = sortBy (compare `on` _tickPrice) rules'
    }

-- | Tries to get 'TickRule' for this price.
tickRuleForPrice :: MonadPlus m
                 => Dictionary
                 -> Price
                 -> m TickRule
tickRuleForPrice d p = case find ((==) p . _tickPrice) (_dTickRules d) of
  Just t  -> return t
  Nothing -> mzero

-----------------------------------------------------------------------------
-- * Order state
-----------------------------------------------------------------------------

-- | Represents an order on the book.
data Order = Order {
    _oOrderId    :: OrderID
  , _oSide       :: Side
  , _oInstrument :: Instrument
  , _oVersions   :: [OrderVersion]
}

data OrderVersion = OrderVersion {

}

-----------------------------------------------------------------------------
-- * Enums
-----------------------------------------------------------------------------

-- | Indicates the side of an order.
data Side
  = Buy  -- ^ Indicates a buy order.
  | Sell -- ^ Indicates a sell order.
  deriving (Eq, Show, Enum)
instance Arbitrary Side where
  arbitrary = elements [Buy, Sell]

-- | Allowed order types.
data OrderType
  = Market -- | 1
  | Limit  -- | 2
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary OrderType where
  arbitrary = elements [Market, Limit]

-- | Allowed time in force values.
data TimeInForce
  = Day -- ^ Day order
  | IOC -- ^ Immediate or cancel
  | GTD -- ^ Good till date
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary TimeInForce where
  arbitrary = elements [Day, IOC, GTD]

-- | Allowed trading phases.
data TradingPhase
  = ContinuousTrading -- ^ Normal trading
  | Closed -- ^ Closed book
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary TradingPhase where
  arbitrary = elements [ContinuousTrading, Closed]

-- | Types of rejections for 'CancelReject'.
data CancelRejectType
  = OrderCancelReject
  | OrderCancelReplaceReject
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary CancelRejectType where
  arbitrary = elements [OrderCancelReject, OrderCancelReplaceReject]

-- | Describes the purpose of the 'ExecutionReport'.
data ExecType
  = EAccepted
  | EPendingNew
  | ERejected
  | EFill
  | EPartialFill
  | ECancelled
  | EPendingCancel
  | EReplace
  | EPendingReplace
  | EExpired
  | EDoneForDay
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary ExecType where
  arbitrary = elements [ EAccepted
                       , EPendingNew
                       , ERejected
                       , EFill
                       , EPartialFill
                       , ECancelled
                       , EPendingCancel
                       , EReplace
                       , EPendingReplace
                       , EExpired
                       , EDoneForDay
                       ]

-- | Identifies current status of 'Order'.
data OrderStatus
  = ONew
  | OPendingNew
  | OFilled
  | OPartiallyFilled
  | OCancelled
  | OPendingCancel
  | ORejected
  | OReplaced
  | OPendingReplace
  | OExpired
  | ODoneForDay
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary OrderStatus where
  arbitrary = elements [ ONew
                       , OPendingNew
                       , OFilled
                       , OPartiallyFilled
                       , OCancelled
                       , OPendingCancel
                       , ORejected
                       , OReplaced
                       , OPendingReplace
                       , OExpired
                       , ODoneForDay
                       ]

-- | Code to identify reason for order rejection.
data OrderRejectReason
  = TooLateToCancel
  | UnknownOrder
  | DuplicateOrder
  | InvalidPriceIncrement
  | IncorrectQuantity
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary OrderRejectReason where
  arbitrary = elements [ TooLateToCancel
                       , UnknownOrder
                       , DuplicateOrder
                       , InvalidPriceIncrement
                       , IncorrectQuantity
                       ]

-----------------------------------------------------------------------------
-- * Outgoing messages
-----------------------------------------------------------------------------

-- | Issued upon receipt of a 'CancelRequest' or 'CancelReplaceRequest'
-- message which cannot be honored.
data CancelReject = CancelReject {
    -- | Type of the 'CancelReject'
    _crType       :: CancelRejectType

    -- | Current status of the 'Order'
  , _crOrdStatus  :: OrderStatus

    -- | Instrument of the 'Order'
  , _crInstrument :: Instrument

    -- | Underlying 'Order'
  , _crOrder      :: Order
}



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
