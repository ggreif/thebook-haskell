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
  , Order, WithOrder, orderL

  -- * Enums
  , Side(..)
  , OrderType(..)
  , OrderRejectReason(..)

  -- * Incoming messages
  , IncomingMessage

  -- * Outgoing messages
  , OutgoingMessage(..)
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
import           Data.Word                 (Word64)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen       (Gen, elements)

-- | Type of id of 'Order'.
type OrderID = ByteString

-- | Client assigned order id.
type ClOrdID = ByteString

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

-- | Monotonically increasing execution id.
type ExecId = Int

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
  , _dCurrency   :: !Currency

    -- | Tick rules of the instrument.
  , _dTickRules  :: ![TickRule]
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
    _oOrderId    :: !OrderID
  , _oSide       :: !Side
  , _oInstrument :: !Instrument
  , _oVersions   :: ![OrderVersion]
}

class WithOrder a where
  orderL :: Lens' a Order

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

-- | Code to identify reason for order rejection in 'ExecutionReport'.
data OrderRejectReason
  = OUnknownSymbol
  | OExchangeClosed
  | OOrderExceedsLimit
  | OTooLateToEnter
  | OUnknownOrder
  | ODuplicateOrder
  | OIncorrectQuantity
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary OrderRejectReason where
  arbitrary = elements [ OUnknownSymbol
                       , OExchangeClosed
                       , OOrderExceedsLimit
                       , OTooLateToEnter
                       , OUnknownOrder
                       , ODuplicateOrder
                       , OIncorrectQuantity
                       ]

-- | Code to identify reason for rejection in 'CancelReject'.
data CancelRejectReason
  = CTooLateToCancel
  | CUnknownOrder
  deriving (Eq, Show, Enum, Bounded)
instance Arbitrary CancelRejectReason where
  arbitrary = elements [ CTooLateToCancel
                       , CUnknownOrder
                       ]

-----------------------------------------------------------------------------
-- * Incoming messages
-----------------------------------------------------------------------------

data IncomingMessage
    -- | The 'OrderCancelReplaceRequest' is used to change
    -- the parameters of an existing order.
  = OrderCancelReplaceRequest {
      _ocrrOrderId     :: !OrderID
    , _ocrrInstrument  :: !Instrument
    , _ocrrSide        :: !Side
    , _ocrrQty         :: !(Maybe Qty)
    , _ocrrOrderType   :: !OrderType
    , _ocrrPrice       :: !(Maybe Price)
    , _ocrrTimeInForce :: !(Maybe TimeInForce)
    , _ocrrExpireTime  :: !(Maybe Time) }
  | NewOrderSingle {
      _nsClOrdID :: ClOrdID
    , _nsPrice   :: Price }

-----------------------------------------------------------------------------
-- * Outgoing messages
-----------------------------------------------------------------------------

data OutgoingMessage
    -- | Issued upon receipt of a 'CancelRequest' or 'CancelReplaceRequest'
    -- message which cannot be honored.
  = CancelReject {
    _crOrder        :: !(Maybe Order)
  , _crType         :: !CancelRejectType
  , _crInstrument   :: !Instrument
  , _crRejectReason :: !CancelRejectReason }

    -- | The 'ExecutionReport' message is used to:
    -- * confirm the receipt of an order.
    -- * confirm changes to an existing order (i.e. accept cancel and replace requests)
    -- * relay order status information.
    -- * relay fill information on working orders.
    -- * reject orders.
  | ExecutionReport {
       _erTimestamp :: !Time
    , _erExecId     :: !ExecId
    , _erExecType   :: !ExecType
    , _erOrder      :: !Order
    , _erPrice      :: !Price
    , _erQty        :: !Qty }




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
