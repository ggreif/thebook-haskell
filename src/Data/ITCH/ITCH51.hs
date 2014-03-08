module Data.ITCH.ITCH51 (ITCHMessage(..)) where
import Data.ITCH.Types
import Data.Decimal
import Data.ByteString.Char8
import Data.Time.Calendar
import Data.Time.Clock
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Binary.Put
import Data.Binary.Get
import Control.Applicative
 
arbitraryAddOrder :: Gen ITCHMessage
arbitraryAddOrder
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>) ((<*>) ((<$>) AddOrder arbitrary) arbitrary) arbitrary)
                     arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryAddAttributedOrder :: Gen ITCHMessage
arbitraryAddAttributedOrder
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>)
                        ((<*>) ((<*>) ((<$>) AddAttributedOrder arbitrary) arbitrary)
                           arbitrary)
                        arbitrary)
                     arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryOrderDeleted :: Gen ITCHMessage
arbitraryOrderDeleted
  = (<*>) ((<*>) ((<$>) OrderDeleted arbitrary) arbitrary) arbitrary
 
arbitraryOrderModified :: Gen ITCHMessage
arbitraryOrderModified
  = (<*>)
      ((<*>)
         ((<*>) ((<*>) ((<$>) OrderModified arbitrary) arbitrary) arbitrary)
         arbitrary)
      arbitrary
 
arbitraryOrderBookClear :: Gen ITCHMessage
arbitraryOrderBookClear
  = (<*>)
      ((<*>)
         ((<*>) ((<*>) ((<$>) OrderBookClear arbitrary) arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryTime :: Gen ITCHMessage
arbitraryTime = (<$>) Time arbitrary
 
arbitraryLoginRequest :: Gen ITCHMessage
arbitraryLoginRequest
  = (<*>) ((<$>) LoginRequest arbitrary) arbitrary
 
arbitraryReplayRequest :: Gen ITCHMessage
arbitraryReplayRequest
  = (<*>) ((<*>) ((<$>) ReplayRequest arbitrary) arbitrary) arbitrary
 
arbitrarySnapshotRequest :: Gen ITCHMessage
arbitrarySnapshotRequest
  = (<*>) ((<*>) ((<$>) SnapshotRequest arbitrary) arbitrary)
      arbitrary
 
arbitraryLogoutRequest :: Gen ITCHMessage
arbitraryLogoutRequest = pure LogoutRequest
 
arbitraryLoginResponse :: Gen ITCHMessage
arbitraryLoginResponse = (<$>) LoginResponse arbitrary
 
arbitraryReplayResponse :: Gen ITCHMessage
arbitraryReplayResponse
  = (<*>)
      ((<*>) ((<*>) ((<$>) ReplayResponse arbitrary) arbitrary)
         arbitrary)
      arbitrary
 
arbitrarySnapshotResponse :: Gen ITCHMessage
arbitrarySnapshotResponse
  = (<*>) ((<*>) ((<$>) SnapshotResponse arbitrary) arbitrary)
      arbitrary
 
arbitrarySnapshotComplete :: Gen ITCHMessage
arbitrarySnapshotComplete
  = (<*>)
      ((<*>) ((<*>) ((<$>) SnapshotComplete arbitrary) arbitrary)
         arbitrary)
      arbitrary
 
arbitrarySystemEvent :: Gen ITCHMessage
arbitrarySystemEvent
  = (<*>) ((<$>) SystemEvent arbitrary) arbitrary
 
arbitrarySymbolDirectory :: Gen ITCHMessage
arbitrarySymbolDirectory
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>)
                        ((<*>)
                           ((<*>)
                              ((<*>)
                                 ((<*>) ((<*>) ((<$>) SymbolDirectory arbitrary) arbitrary)
                                    arbitrary)
                                 arbitrary)
                              arbitrary)
                           arbitrary)
                        arbitrary)
                     arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitrarySymbolStatus :: Gen ITCHMessage
arbitrarySymbolStatus
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>)
                        ((<*>) ((<*>) ((<$>) SymbolStatus arbitrary) arbitrary) arbitrary)
                        arbitrary)
                     arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryOrderExecuted :: Gen ITCHMessage
arbitraryOrderExecuted
  = (<*>)
      ((<*>) ((<*>) ((<$>) OrderExecuted arbitrary) arbitrary) arbitrary)
      arbitrary
 
arbitraryOrderExecutedWithPrice :: Gen ITCHMessage
arbitraryOrderExecutedWithPrice
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>) ((<*>) ((<$>) OrderExecutedWithPrice arbitrary) arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryTrade :: Gen ITCHMessage
arbitraryTrade
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>) ((<*>) ((<*>) ((<$>) Trade arbitrary) arbitrary) arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryAuctionTrade :: Gen ITCHMessage
arbitraryAuctionTrade
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>) ((<*>) ((<$>) AuctionTrade arbitrary) arbitrary) arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryTradeBreak :: Gen ITCHMessage
arbitraryTradeBreak
  = (<*>) ((<*>) ((<$>) TradeBreak arbitrary) arbitrary) arbitrary
 
arbitraryAuctionInfo :: Gen ITCHMessage
arbitraryAuctionInfo
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>) ((<*>) ((<$>) AuctionInfo arbitrary) arbitrary) arbitrary)
                     arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryOffBookTrade :: Gen ITCHMessage
arbitraryOffBookTrade
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>)
                        ((<*>)
                           ((<*>)
                              ((<*>)
                                 ((<*>)
                                    ((<*>) ((<*>) ((<$>) OffBookTrade arbitrary) arbitrary)
                                       arbitrary)
                                    arbitrary)
                                 arbitrary)
                              arbitrary)
                           arbitrary)
                        arbitrary)
                     arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
arbitraryStatistics :: Gen ITCHMessage
arbitraryStatistics
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>) ((<*>) ((<$>) Statistics arbitrary) arbitrary) arbitrary)
                  arbitrary)
               arbitrary)
            arbitrary)
         arbitrary)
      arbitrary
 
instance Arbitrary ITCHMessage where
        arbitrary
          = oneof
              [ arbitraryAddOrder,  arbitraryAddAttributedOrder,
                arbitraryOrderDeleted,  arbitraryOrderModified,
                arbitraryOrderBookClear,  arbitraryTime,  arbitraryLoginRequest,
                arbitraryReplayRequest,  arbitrarySnapshotRequest,
                arbitraryLogoutRequest,  arbitraryLoginResponse,
                arbitraryReplayResponse,  arbitrarySnapshotResponse,
                arbitrarySnapshotComplete,  arbitrarySystemEvent,
                arbitrarySymbolDirectory,  arbitrarySymbolStatus,
                arbitraryOrderExecuted,  arbitraryOrderExecutedWithPrice,
                arbitraryTrade,  arbitraryAuctionTrade,  arbitraryTradeBreak,
                arbitraryAuctionInfo,  arbitraryOffBookTrade,
                arbitraryStatistics]
 
data ITCHMessage = AddOrder{_addOrderNanosecond ::
                            {-# UNPACK #-} !UInt32,
                            _addOrderOrderID :: {-# UNPACK #-} !UInt64,
                            _addOrderSide :: {-# UNPACK #-} !Byte,
                            _addOrderQuantity :: {-# UNPACK #-} !UInt32,
                            _addOrderLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                            _addOrderReserved1 :: {-# UNPACK #-} !Byte,
                            _addOrderReserved2 :: {-# UNPACK #-} !Byte,
                            _addOrderPrice :: {-# UNPACK #-} !Price,
                            _addOrderFlags :: {-# UNPACK #-} !BitField}
                 | AddAttributedOrder{_addAttributedOrderNanosecond ::
                                      {-# UNPACK #-} !UInt32,
                                      _addAttributedOrderOrderID :: {-# UNPACK #-} !UInt64,
                                      _addAttributedOrderSide :: {-# UNPACK #-} !Byte,
                                      _addAttributedOrderQuantity :: {-# UNPACK #-} !UInt32,
                                      _addAttributedOrderLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                                      _addAttributedOrderReserved1 :: {-# UNPACK #-} !Byte,
                                      _addAttributedOrderReserved2 :: {-# UNPACK #-} !Byte,
                                      _addAttributedOrderPrice :: {-# UNPACK #-} !Price,
                                      _addAttributedOrderAttribution :: {-# UNPACK #-} !Alpha,
                                      _addAttributedOrderFlags :: {-# UNPACK #-} !BitField}
                 | OrderDeleted{_orderDeletedNanosecond :: {-# UNPACK #-} !UInt32,
                                _orderDeletedOrderID :: {-# UNPACK #-} !UInt64,
                                _orderDeletedFlags :: {-# UNPACK #-} !BitField}
                 | OrderModified{_orderModifiedNanosecond :: {-# UNPACK #-} !UInt32,
                                 _orderModifiedOrderID :: {-# UNPACK #-} !UInt64,
                                 _orderModifiedNewQuantity :: {-# UNPACK #-} !UInt32,
                                 _orderModifiedNewPrice :: {-# UNPACK #-} !Price,
                                 _orderModifiedFlags :: {-# UNPACK #-} !BitField}
                 | OrderBookClear{_orderBookClearNanosecond ::
                                  {-# UNPACK #-} !UInt32,
                                  _orderBookClearLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                                  _orderBookClearReserved1 :: {-# UNPACK #-} !Byte,
                                  _orderBookClearReserved2 :: {-# UNPACK #-} !Byte,
                                  _orderBookClearFlags :: {-# UNPACK #-} !BitField}
                 | Time{_timeSeconds :: {-# UNPACK #-} !UInt32}
                 | LoginRequest{_loginRequestUsername :: {-# UNPACK #-} !Alpha,
                                _loginRequestPassword :: {-# UNPACK #-} !Alpha}
                 | ReplayRequest{_replayRequestMarketDataGroup ::
                                 {-# UNPACK #-} !Byte,
                                 _replayRequestFirstMessage :: {-# UNPACK #-} !UInt32,
                                 _replayRequestCount :: {-# UNPACK #-} !UInt8}
                 | SnapshotRequest{_snapshotRequestSequenceNumber ::
                                   {-# UNPACK #-} !UInt32,
                                   _snapshotRequestSegment :: {-# UNPACK #-} !Alpha,
                                   _snapshotRequestLSEInstrumentID :: {-# UNPACK #-} !UInt32}
                 | LogoutRequest{}
                 | LoginResponse{_loginResponseStatus :: {-# UNPACK #-} !Byte}
                 | ReplayResponse{_replayResponseMarketDataGroup ::
                                  {-# UNPACK #-} !Byte,
                                  _replayResponseFirstMessage :: {-# UNPACK #-} !UInt32,
                                  _replayResponseCount :: {-# UNPACK #-} !UInt8,
                                  _replayResponseStatus :: {-# UNPACK #-} !Byte}
                 | SnapshotResponse{_snapshotResponseSequenceNumber ::
                                    {-# UNPACK #-} !UInt32,
                                    _snapshotResponseOrderCount :: {-# UNPACK #-} !UInt32,
                                    _snapshotResponseStatus :: {-# UNPACK #-} !Byte}
                 | SnapshotComplete{_snapshotCompleteSequenceNumber ::
                                    {-# UNPACK #-} !UInt32,
                                    _snapshotCompleteSegment :: {-# UNPACK #-} !Alpha,
                                    _snapshotCompleteLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                                    _snapshotCompleteFlags :: {-# UNPACK #-} !BitField}
                 | SystemEvent{_systemEventNanosecond :: {-# UNPACK #-} !UInt32,
                               _systemEventEventCode :: {-# UNPACK #-} !Byte}
                 | SymbolDirectory{_symbolDirectoryNanosecond ::
                                   {-# UNPACK #-} !UInt32,
                                   _symbolDirectoryLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                                   _symbolDirectoryReserved1 :: {-# UNPACK #-} !Byte,
                                   _symbolDirectoryReserved2 :: {-# UNPACK #-} !Byte,
                                   _symbolDirectorySymbolStatus :: {-# UNPACK #-} !Alpha,
                                   _symbolDirectoryISIN :: {-# UNPACK #-} !Alpha,
                                   _symbolDirectorySEDOL :: {-# UNPACK #-} !Alpha,
                                   _symbolDirectorySegment :: {-# UNPACK #-} !Alpha,
                                   _symbolDirectoryUnderlying :: {-# UNPACK #-} !Alpha,
                                   _symbolDirectoryCurrency :: {-# UNPACK #-} !Alpha,
                                   _symbolDirectoryTargetBook :: {-# UNPACK #-} !Byte,
                                   _symbolDirectorySecurityExchange :: {-# UNPACK #-} !Alpha,
                                   _symbolDirectoryPreviousClosePrice :: {-# UNPACK #-} !Price}
                 | SymbolStatus{_symbolStatusNanosecond :: {-# UNPACK #-} !UInt32,
                                _symbolStatusLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                                _symbolStatusReserved1 :: {-# UNPACK #-} !Byte,
                                _symbolStatusReserved2 :: {-# UNPACK #-} !Byte,
                                _symbolStatusTradingStatus :: {-# UNPACK #-} !Byte,
                                _symbolStatusFlags :: {-# UNPACK #-} !BitField,
                                _symbolStatusHaltReason :: {-# UNPACK #-} !Alpha,
                                _symbolStatusSessionChangeReason :: {-# UNPACK #-} !UInt8,
                                _symbolStatusNewEndTime :: {-# UNPACK #-} !Time,
                                _symbolStatusBookType :: {-# UNPACK #-} !UInt8}
                 | OrderExecuted{_orderExecutedNanosecond :: {-# UNPACK #-} !UInt32,
                                 _orderExecutedOrderID :: {-# UNPACK #-} !UInt64,
                                 _orderExecutedExecutedQuantity :: {-# UNPACK #-} !UInt32,
                                 _orderExecutedTradeID :: {-# UNPACK #-} !UInt64}
                 | OrderExecutedWithPrice{_orderExecutedWithPriceNanosecond ::
                                          {-# UNPACK #-} !UInt32,
                                          _orderExecutedWithPriceOrderID :: {-# UNPACK #-} !UInt64,
                                          _orderExecutedWithPriceExecutedQuantity ::
                                          {-# UNPACK #-} !UInt32,
                                          _orderExecutedWithPriceDisplayQuantity ::
                                          {-# UNPACK #-} !UInt32,
                                          _orderExecutedWithPriceTradeID :: {-# UNPACK #-} !UInt64,
                                          _orderExecutedWithPricePrintable :: {-# UNPACK #-} !Byte,
                                          _orderExecutedWithPricePrice :: {-# UNPACK #-} !Price}
                 | Trade{_tradeNanosecond :: {-# UNPACK #-} !UInt32,
                         _tradeExecutedQuantity :: {-# UNPACK #-} !UInt32,
                         _tradeLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                         _tradeReserved1 :: {-# UNPACK #-} !Byte,
                         _tradeReserved2 :: {-# UNPACK #-} !Byte,
                         _tradePrice :: {-# UNPACK #-} !Price,
                         _tradeTradeID :: {-# UNPACK #-} !UInt64,
                         _tradeSideOfAggressor :: {-# UNPACK #-} !Byte}
                 | AuctionTrade{_auctionTradeNanosecond :: {-# UNPACK #-} !UInt32,
                                _auctionTradeQuantity :: {-# UNPACK #-} !UInt32,
                                _auctionTradeLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                                _auctionTradeReserved1 :: {-# UNPACK #-} !Byte,
                                _auctionTradeReserved2 :: {-# UNPACK #-} !Byte,
                                _auctionTradePrice :: {-# UNPACK #-} !Price,
                                _auctionTradeTradeID :: {-# UNPACK #-} !UInt64,
                                _auctionTradeAuctionType :: {-# UNPACK #-} !Byte}
                 | TradeBreak{_tradeBreakNanosecond :: {-# UNPACK #-} !UInt32,
                              _tradeBreakTradeID :: {-# UNPACK #-} !UInt64,
                              _tradeBreakTradeType :: {-# UNPACK #-} !Byte}
                 | AuctionInfo{_auctionInfoNanosecond :: {-# UNPACK #-} !UInt32,
                               _auctionInfoPairedQuantity :: {-# UNPACK #-} !UInt32,
                               _auctionInfoImbalanceQuantity :: {-# UNPACK #-} !UInt32,
                               _auctionInfoImbalanceDirection :: {-# UNPACK #-} !Byte,
                               _auctionInfoLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                               _auctionInfoReserved1 :: {-# UNPACK #-} !Byte,
                               _auctionInfoReserved2 :: {-# UNPACK #-} !Byte,
                               _auctionInfoPrice :: {-# UNPACK #-} !Price,
                               _auctionInfoAuctionType :: {-# UNPACK #-} !Byte}
                 | OffBookTrade{_offBookTradeNanosecond :: {-# UNPACK #-} !UInt32,
                                _offBookTradeExecutedQuantity :: {-# UNPACK #-} !UInt32,
                                _offBookTradeLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                                _offBookTradeReserved1 :: {-# UNPACK #-} !Byte,
                                _offBookTradeReserved2 :: {-# UNPACK #-} !Byte,
                                _offBookTradePrice :: {-# UNPACK #-} !Price,
                                _offBookTradeTradeID :: {-# UNPACK #-} !UInt64,
                                _offBookTradeOffBookTradeType :: {-# UNPACK #-} !Alpha,
                                _offBookTradeTradeTime :: {-# UNPACK #-} !Time,
                                _offBookTradeTradeDate :: {-# UNPACK #-} !Date,
                                _offBookTradeTradedCurrency :: {-# UNPACK #-} !Alpha,
                                _offBookTradeOriginalPrice :: {-# UNPACK #-} !Price,
                                _offBookTradeExecutionVenue :: {-# UNPACK #-} !Alpha,
                                _offBookTradeFlags :: {-# UNPACK #-} !BitField}
                 | Statistics{_statisticsNanosecond :: {-# UNPACK #-} !UInt32,
                              _statisticsLSEInstrumentID :: {-# UNPACK #-} !UInt32,
                              _statisticsReserved1 :: {-# UNPACK #-} !Byte,
                              _statisticsReserved2 :: {-# UNPACK #-} !Byte,
                              _statisticsLSEGStatisticsType :: {-# UNPACK #-} !Alpha,
                              _statisticsPrice :: {-# UNPACK #-} !Price,
                              _statisticsOpenClosePriceIndicator :: {-# UNPACK #-} !Alpha,
                              _statisticsFlags :: {-# UNPACK #-} !BitField}
                 deriving (Show, Eq)