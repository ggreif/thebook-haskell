module Data.ITCH.ITCH51 (ITCHMessage(..)) where
import qualified Data.ITCH.Types as Data.ITCH.Types
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Control.Applicative
 
getAddOrder :: Get ITCHMessage
getAddOrder
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>) ((<*>) ((<*>) ((<*>) ((<$>) AddOrder get) get) get) get)
                  get)
               get)
            get)
         get)
      get
 
getAddAttributedOrder :: Get ITCHMessage
getAddAttributedOrder
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>) ((<*>) ((<*>) ((<$>) AddAttributedOrder get) get) get) get)
                     get)
                  get)
               get)
            get)
         (Data.ITCH.Types.getAlpha 11))
      get
 
getOrderDeleted :: Get ITCHMessage
getOrderDeleted = (<*>) ((<*>) ((<$>) OrderDeleted get) get) get
 
getOrderModified :: Get ITCHMessage
getOrderModified
  = (<*>)
      ((<*>) ((<*>) ((<*>) ((<$>) OrderModified get) get) get) get)
      get
 
getOrderBookClear :: Get ITCHMessage
getOrderBookClear
  = (<*>)
      ((<*>) ((<*>) ((<*>) ((<$>) OrderBookClear get) get) get) get)
      get
 
getTime :: Get ITCHMessage
getTime = (<$>) Time get
 
getLoginRequest :: Get ITCHMessage
getLoginRequest
  = (<*>) ((<$>) LoginRequest (Data.ITCH.Types.getAlpha 6))
      (Data.ITCH.Types.getAlpha 10)
 
getReplayRequest :: Get ITCHMessage
getReplayRequest = (<*>) ((<*>) ((<$>) ReplayRequest get) get) get
 
getSnapshotRequest :: Get ITCHMessage
getSnapshotRequest
  = (<*>)
      ((<*>) ((<$>) SnapshotRequest get) (Data.ITCH.Types.getAlpha 6))
      get
 
getLogoutRequest :: Get ITCHMessage
getLogoutRequest = pure LogoutRequest
 
getLoginResponse :: Get ITCHMessage
getLoginResponse = (<$>) LoginResponse get
 
getReplayResponse :: Get ITCHMessage
getReplayResponse
  = (<*>) ((<*>) ((<*>) ((<$>) ReplayResponse get) get) get) get
 
getSnapshotResponse :: Get ITCHMessage
getSnapshotResponse
  = (<*>) ((<*>) ((<$>) SnapshotResponse get) get) get
 
getSnapshotComplete :: Get ITCHMessage
getSnapshotComplete
  = (<*>)
      ((<*>)
         ((<*>) ((<$>) SnapshotComplete get) (Data.ITCH.Types.getAlpha 6))
         get)
      get
 
getSystemEvent :: Get ITCHMessage
getSystemEvent = (<*>) ((<$>) SystemEvent get) get
 
getSymbolDirectory :: Get ITCHMessage
getSymbolDirectory
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>)
                        ((<*>)
                           ((<*>)
                              ((<*>) ((<*>) ((<*>) ((<$>) SymbolDirectory get) get) get) get)
                              (Data.ITCH.Types.getAlpha 1))
                           (Data.ITCH.Types.getAlpha 12))
                        (Data.ITCH.Types.getAlpha 12))
                     (Data.ITCH.Types.getAlpha 6))
                  (Data.ITCH.Types.getAlpha 6))
               (Data.ITCH.Types.getAlpha 3))
            get)
         (Data.ITCH.Types.getAlpha 4))
      get
 
getSymbolStatus :: Get ITCHMessage
getSymbolStatus
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>) ((<*>) ((<*>) ((<*>) ((<$>) SymbolStatus get) get) get) get)
                     get)
                  get)
               (Data.ITCH.Types.getAlpha 4))
            get)
         get)
      get
 
getOrderExecuted :: Get ITCHMessage
getOrderExecuted
  = (<*>) ((<*>) ((<*>) ((<$>) OrderExecuted get) get) get) get
 
getOrderExecutedWithPrice :: Get ITCHMessage
getOrderExecutedWithPrice
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>) ((<*>) ((<*>) ((<$>) OrderExecutedWithPrice get) get) get)
               get)
            get)
         get)
      get
 
getTrade :: Get ITCHMessage
getTrade
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>) ((<*>) ((<*>) ((<*>) ((<$>) Trade get) get) get) get) get)
            get)
         get)
      get
 
getAuctionTrade :: Get ITCHMessage
getAuctionTrade
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>) ((<*>) ((<*>) ((<*>) ((<$>) AuctionTrade get) get) get) get)
               get)
            get)
         get)
      get
 
getTradeBreak :: Get ITCHMessage
getTradeBreak = (<*>) ((<*>) ((<$>) TradeBreak get) get) get
 
getAuctionInfo :: Get ITCHMessage
getAuctionInfo
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>) ((<*>) ((<*>) ((<*>) ((<$>) AuctionInfo get) get) get) get)
                  get)
               get)
            get)
         get)
      get
 
getOffBookTrade :: Get ITCHMessage
getOffBookTrade
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>)
               ((<*>)
                  ((<*>)
                     ((<*>)
                        ((<*>)
                           ((<*>)
                              ((<*>) ((<*>) ((<*>) ((<*>) ((<$>) OffBookTrade get) get) get) get)
                                 get)
                              get)
                           get)
                        (Data.ITCH.Types.getAlpha 4))
                     get)
                  get)
               (Data.ITCH.Types.getAlpha 4))
            get)
         (Data.ITCH.Types.getAlpha 5))
      get
 
getStatistics :: Get ITCHMessage
getStatistics
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>) ((<*>) ((<*>) ((<*>) ((<$>) Statistics get) get) get) get)
               (Data.ITCH.Types.getAlpha 1))
            get)
         (Data.ITCH.Types.getAlpha 1))
      get
 
instance Binary ITCHMessage where
        get
          = do msgLength <- Data.ITCH.Types.getMessageLength
               msgType <- Data.ITCH.Types.getMessageType
               case msgType of
                   65 -> (<*) getAddOrder (Data.ITCH.Types.skipRemaining msgLength 34)
                   70 -> (<*) getAddAttributedOrder
                           (Data.ITCH.Types.skipRemaining msgLength 45)
                   68 -> (<*) getOrderDeleted
                           (Data.ITCH.Types.skipRemaining msgLength 15)
                   85 -> (<*) getOrderModified
                           (Data.ITCH.Types.skipRemaining msgLength 27)
                   121 -> (<*) getOrderBookClear
                            (Data.ITCH.Types.skipRemaining msgLength 13)
                   84 -> (<*) getTime (Data.ITCH.Types.skipRemaining msgLength 6)
                   1 -> (<*) getLoginRequest
                          (Data.ITCH.Types.skipRemaining msgLength 18)
                   3 -> (<*) getReplayRequest
                          (Data.ITCH.Types.skipRemaining msgLength 9)
                   129 -> (<*) getSnapshotRequest
                            (Data.ITCH.Types.skipRemaining msgLength 16)
                   5 -> (<*) getLogoutRequest
                          (Data.ITCH.Types.skipRemaining msgLength 2)
                   2 -> (<*) getLoginResponse
                          (Data.ITCH.Types.skipRemaining msgLength 3)
                   4 -> (<*) getReplayResponse
                          (Data.ITCH.Types.skipRemaining msgLength 10)
                   130 -> (<*) getSnapshotResponse
                            (Data.ITCH.Types.skipRemaining msgLength 11)
                   131 -> (<*) getSnapshotComplete
                            (Data.ITCH.Types.skipRemaining msgLength 17)
                   83 -> (<*) getSystemEvent
                           (Data.ITCH.Types.skipRemaining msgLength 7)
                   82 -> (<*) getSymbolDirectory
                           (Data.ITCH.Types.skipRemaining msgLength 65)
                   72 -> (<*) getSymbolStatus
                           (Data.ITCH.Types.skipRemaining msgLength 28)
                   69 -> (<*) getOrderExecuted
                           (Data.ITCH.Types.skipRemaining msgLength 26)
                   67 -> (<*) getOrderExecutedWithPrice
                           (Data.ITCH.Types.skipRemaining msgLength 39)
                   80 -> (<*) getTrade (Data.ITCH.Types.skipRemaining msgLength 33)
                   81 -> (<*) getAuctionTrade
                           (Data.ITCH.Types.skipRemaining msgLength 33)
                   66 -> (<*) getTradeBreak
                           (Data.ITCH.Types.skipRemaining msgLength 15)
                   73 -> (<*) getAuctionInfo
                           (Data.ITCH.Types.skipRemaining msgLength 30)
                   120 -> (<*) getOffBookTrade
                            (Data.ITCH.Types.skipRemaining msgLength 70)
                   119 -> (<*) getStatistics
                            (Data.ITCH.Types.skipRemaining msgLength 23)
                   _ -> fail "Unknown msg type"
        put msg@AddOrder{}
          = (*>) (Data.ITCH.Types.putMessageLength 34)
              ((*>) (Data.ITCH.Types.putMessageType 65)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>)
                                      ((*>) (put (_addOrderNanosecond msg))
                                         (put (_addOrderOrderID msg)))
                                      (put (_addOrderSide msg)))
                                   (put (_addOrderQuantity msg)))
                                (put (_addOrderLSEInstrumentID msg)))
                             (put (_addOrderReserved1 msg)))
                          (put (_addOrderReserved2 msg)))
                       (put (_addOrderPrice msg)))
                    (put (_addOrderFlags msg))))
        put msg@AddAttributedOrder{}
          = (*>) (Data.ITCH.Types.putMessageLength 45)
              ((*>) (Data.ITCH.Types.putMessageType 70)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>)
                                      ((*>)
                                         ((*>) (put (_addAttributedOrderNanosecond msg))
                                            (put (_addAttributedOrderOrderID msg)))
                                         (put (_addAttributedOrderSide msg)))
                                      (put (_addAttributedOrderQuantity msg)))
                                   (put (_addAttributedOrderLSEInstrumentID msg)))
                                (put (_addAttributedOrderReserved1 msg)))
                             (put (_addAttributedOrderReserved2 msg)))
                          (put (_addAttributedOrderPrice msg)))
                       (Data.ITCH.Types.putAlpha 11 (_addAttributedOrderAttribution msg)))
                    (put (_addAttributedOrderFlags msg))))
        put msg@OrderDeleted{}
          = (*>) (Data.ITCH.Types.putMessageLength 15)
              ((*>) (Data.ITCH.Types.putMessageType 68)
                 ((*>)
                    ((*>) (put (_orderDeletedNanosecond msg))
                       (put (_orderDeletedOrderID msg)))
                    (put (_orderDeletedFlags msg))))
        put msg@OrderModified{}
          = (*>) (Data.ITCH.Types.putMessageLength 27)
              ((*>) (Data.ITCH.Types.putMessageType 85)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>) (put (_orderModifiedNanosecond msg))
                             (put (_orderModifiedOrderID msg)))
                          (put (_orderModifiedNewQuantity msg)))
                       (put (_orderModifiedNewPrice msg)))
                    (put (_orderModifiedFlags msg))))
        put msg@OrderBookClear{}
          = (*>) (Data.ITCH.Types.putMessageLength 13)
              ((*>) (Data.ITCH.Types.putMessageType 121)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>) (put (_orderBookClearNanosecond msg))
                             (put (_orderBookClearLSEInstrumentID msg)))
                          (put (_orderBookClearReserved1 msg)))
                       (put (_orderBookClearReserved2 msg)))
                    (put (_orderBookClearFlags msg))))
        put msg@Time{}
          = (*>) (Data.ITCH.Types.putMessageLength 6)
              ((*>) (Data.ITCH.Types.putMessageType 84) (put (_timeSeconds msg)))
        put msg@LoginRequest{}
          = (*>) (Data.ITCH.Types.putMessageLength 18)
              ((*>) (Data.ITCH.Types.putMessageType 1)
                 ((*>) (Data.ITCH.Types.putAlpha 6 (_loginRequestUsername msg))
                    (Data.ITCH.Types.putAlpha 10 (_loginRequestPassword msg))))
        put msg@ReplayRequest{}
          = (*>) (Data.ITCH.Types.putMessageLength 9)
              ((*>) (Data.ITCH.Types.putMessageType 3)
                 ((*>)
                    ((*>) (put (_replayRequestMarketDataGroup msg))
                       (put (_replayRequestFirstMessage msg)))
                    (put (_replayRequestCount msg))))
        put msg@SnapshotRequest{}
          = (*>) (Data.ITCH.Types.putMessageLength 16)
              ((*>) (Data.ITCH.Types.putMessageType 129)
                 ((*>)
                    ((*>) (put (_snapshotRequestSequenceNumber msg))
                       (Data.ITCH.Types.putAlpha 6 (_snapshotRequestSegment msg)))
                    (put (_snapshotRequestLSEInstrumentID msg))))
        put msg@LogoutRequest{}
          = (*>) (Data.ITCH.Types.putMessageLength 2)
              ((*>) (Data.ITCH.Types.putMessageType 5) (return ()))
        put msg@LoginResponse{}
          = (*>) (Data.ITCH.Types.putMessageLength 3)
              ((*>) (Data.ITCH.Types.putMessageType 2)
                 (put (_loginResponseStatus msg)))
        put msg@ReplayResponse{}
          = (*>) (Data.ITCH.Types.putMessageLength 10)
              ((*>) (Data.ITCH.Types.putMessageType 4)
                 ((*>)
                    ((*>)
                       ((*>) (put (_replayResponseMarketDataGroup msg))
                          (put (_replayResponseFirstMessage msg)))
                       (put (_replayResponseCount msg)))
                    (put (_replayResponseStatus msg))))
        put msg@SnapshotResponse{}
          = (*>) (Data.ITCH.Types.putMessageLength 11)
              ((*>) (Data.ITCH.Types.putMessageType 130)
                 ((*>)
                    ((*>) (put (_snapshotResponseSequenceNumber msg))
                       (put (_snapshotResponseOrderCount msg)))
                    (put (_snapshotResponseStatus msg))))
        put msg@SnapshotComplete{}
          = (*>) (Data.ITCH.Types.putMessageLength 17)
              ((*>) (Data.ITCH.Types.putMessageType 131)
                 ((*>)
                    ((*>)
                       ((*>) (put (_snapshotCompleteSequenceNumber msg))
                          (Data.ITCH.Types.putAlpha 6 (_snapshotCompleteSegment msg)))
                       (put (_snapshotCompleteLSEInstrumentID msg)))
                    (put (_snapshotCompleteFlags msg))))
        put msg@SystemEvent{}
          = (*>) (Data.ITCH.Types.putMessageLength 7)
              ((*>) (Data.ITCH.Types.putMessageType 83)
                 ((*>) (put (_systemEventNanosecond msg))
                    (put (_systemEventEventCode msg))))
        put msg@SymbolDirectory{}
          = (*>) (Data.ITCH.Types.putMessageLength 65)
              ((*>) (Data.ITCH.Types.putMessageType 82)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>)
                                      ((*>)
                                         ((*>)
                                            ((*>)
                                               ((*>)
                                                  ((*>) (put (_symbolDirectoryNanosecond msg))
                                                     (put (_symbolDirectoryLSEInstrumentID msg)))
                                                  (put (_symbolDirectoryReserved1 msg)))
                                               (put (_symbolDirectoryReserved2 msg)))
                                            (Data.ITCH.Types.putAlpha 1
                                               (_symbolDirectorySymbolStatus msg)))
                                         (Data.ITCH.Types.putAlpha 12 (_symbolDirectoryISIN msg)))
                                      (Data.ITCH.Types.putAlpha 12 (_symbolDirectorySEDOL msg)))
                                   (Data.ITCH.Types.putAlpha 6 (_symbolDirectorySegment msg)))
                                (Data.ITCH.Types.putAlpha 6 (_symbolDirectoryUnderlying msg)))
                             (Data.ITCH.Types.putAlpha 3 (_symbolDirectoryCurrency msg)))
                          (put (_symbolDirectoryTargetBook msg)))
                       (Data.ITCH.Types.putAlpha 4
                          (_symbolDirectorySecurityExchange msg)))
                    (put (_symbolDirectoryPreviousClosePrice msg))))
        put msg@SymbolStatus{}
          = (*>) (Data.ITCH.Types.putMessageLength 28)
              ((*>) (Data.ITCH.Types.putMessageType 72)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>)
                                      ((*>)
                                         ((*>) (put (_symbolStatusNanosecond msg))
                                            (put (_symbolStatusLSEInstrumentID msg)))
                                         (put (_symbolStatusReserved1 msg)))
                                      (put (_symbolStatusReserved2 msg)))
                                   (put (_symbolStatusTradingStatus msg)))
                                (put (_symbolStatusFlags msg)))
                             (Data.ITCH.Types.putAlpha 4 (_symbolStatusHaltReason msg)))
                          (put (_symbolStatusSessionChangeReason msg)))
                       (put (_symbolStatusNewEndTime msg)))
                    (put (_symbolStatusBookType msg))))
        put msg@OrderExecuted{}
          = (*>) (Data.ITCH.Types.putMessageLength 26)
              ((*>) (Data.ITCH.Types.putMessageType 69)
                 ((*>)
                    ((*>)
                       ((*>) (put (_orderExecutedNanosecond msg))
                          (put (_orderExecutedOrderID msg)))
                       (put (_orderExecutedExecutedQuantity msg)))
                    (put (_orderExecutedTradeID msg))))
        put msg@OrderExecutedWithPrice{}
          = (*>) (Data.ITCH.Types.putMessageLength 39)
              ((*>) (Data.ITCH.Types.putMessageType 67)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>) (put (_orderExecutedWithPriceNanosecond msg))
                                   (put (_orderExecutedWithPriceOrderID msg)))
                                (put (_orderExecutedWithPriceExecutedQuantity msg)))
                             (put (_orderExecutedWithPriceDisplayQuantity msg)))
                          (put (_orderExecutedWithPriceTradeID msg)))
                       (put (_orderExecutedWithPricePrintable msg)))
                    (put (_orderExecutedWithPricePrice msg))))
        put msg@Trade{}
          = (*>) (Data.ITCH.Types.putMessageLength 33)
              ((*>) (Data.ITCH.Types.putMessageType 80)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>) (put (_tradeNanosecond msg))
                                      (put (_tradeExecutedQuantity msg)))
                                   (put (_tradeLSEInstrumentID msg)))
                                (put (_tradeReserved1 msg)))
                             (put (_tradeReserved2 msg)))
                          (put (_tradePrice msg)))
                       (put (_tradeTradeID msg)))
                    (put (_tradeSideOfAggressor msg))))
        put msg@AuctionTrade{}
          = (*>) (Data.ITCH.Types.putMessageLength 33)
              ((*>) (Data.ITCH.Types.putMessageType 81)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>) (put (_auctionTradeNanosecond msg))
                                      (put (_auctionTradeQuantity msg)))
                                   (put (_auctionTradeLSEInstrumentID msg)))
                                (put (_auctionTradeReserved1 msg)))
                             (put (_auctionTradeReserved2 msg)))
                          (put (_auctionTradePrice msg)))
                       (put (_auctionTradeTradeID msg)))
                    (put (_auctionTradeAuctionType msg))))
        put msg@TradeBreak{}
          = (*>) (Data.ITCH.Types.putMessageLength 15)
              ((*>) (Data.ITCH.Types.putMessageType 66)
                 ((*>)
                    ((*>) (put (_tradeBreakNanosecond msg))
                       (put (_tradeBreakTradeID msg)))
                    (put (_tradeBreakTradeType msg))))
        put msg@AuctionInfo{}
          = (*>) (Data.ITCH.Types.putMessageLength 30)
              ((*>) (Data.ITCH.Types.putMessageType 73)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>)
                                      ((*>) (put (_auctionInfoNanosecond msg))
                                         (put (_auctionInfoPairedQuantity msg)))
                                      (put (_auctionInfoImbalanceQuantity msg)))
                                   (put (_auctionInfoImbalanceDirection msg)))
                                (put (_auctionInfoLSEInstrumentID msg)))
                             (put (_auctionInfoReserved1 msg)))
                          (put (_auctionInfoReserved2 msg)))
                       (put (_auctionInfoPrice msg)))
                    (put (_auctionInfoAuctionType msg))))
        put msg@OffBookTrade{}
          = (*>) (Data.ITCH.Types.putMessageLength 70)
              ((*>) (Data.ITCH.Types.putMessageType 120)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>)
                                      ((*>)
                                         ((*>)
                                            ((*>)
                                               ((*>)
                                                  ((*>)
                                                     ((*>) (put (_offBookTradeNanosecond msg))
                                                        (put (_offBookTradeExecutedQuantity msg)))
                                                     (put (_offBookTradeLSEInstrumentID msg)))
                                                  (put (_offBookTradeReserved1 msg)))
                                               (put (_offBookTradeReserved2 msg)))
                                            (put (_offBookTradePrice msg)))
                                         (put (_offBookTradeTradeID msg)))
                                      (Data.ITCH.Types.putAlpha 4
                                         (_offBookTradeOffBookTradeType msg)))
                                   (put (_offBookTradeTradeTime msg)))
                                (put (_offBookTradeTradeDate msg)))
                             (Data.ITCH.Types.putAlpha 4 (_offBookTradeTradedCurrency msg)))
                          (put (_offBookTradeOriginalPrice msg)))
                       (Data.ITCH.Types.putAlpha 5 (_offBookTradeExecutionVenue msg)))
                    (put (_offBookTradeFlags msg))))
        put msg@Statistics{}
          = (*>) (Data.ITCH.Types.putMessageLength 23)
              ((*>) (Data.ITCH.Types.putMessageType 119)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>) (put (_statisticsNanosecond msg))
                                      (put (_statisticsLSEInstrumentID msg)))
                                   (put (_statisticsReserved1 msg)))
                                (put (_statisticsReserved2 msg)))
                             (Data.ITCH.Types.putAlpha 1 (_statisticsLSEGStatisticsType msg)))
                          (put (_statisticsPrice msg)))
                       (Data.ITCH.Types.putAlpha 1
                          (_statisticsOpenClosePriceIndicator msg)))
                    (put (_statisticsFlags msg))))
        put _ = fail "Unknown msg type"
 
instance Arbitrary ITCHMessage where
        arbitrary
          = oneof
              [arbitraryAddOrder, arbitraryAddAttributedOrder,
               arbitraryOrderDeleted, arbitraryOrderModified,
               arbitraryOrderBookClear, arbitraryTime, arbitraryLoginRequest,
               arbitraryReplayRequest, arbitrarySnapshotRequest,
               arbitraryLogoutRequest, arbitraryLoginResponse,
               arbitraryReplayResponse, arbitrarySnapshotResponse,
               arbitrarySnapshotComplete, arbitrarySystemEvent,
               arbitrarySymbolDirectory, arbitrarySymbolStatus,
               arbitraryOrderExecuted, arbitraryOrderExecutedWithPrice,
               arbitraryTrade, arbitraryAuctionTrade, arbitraryTradeBreak,
               arbitraryAuctionInfo, arbitraryOffBookTrade, arbitraryStatistics]
 
data ITCHMessage = AddOrder{_addOrderNanosecond ::
                            {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                            _addOrderOrderID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                            _addOrderSide :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                            _addOrderQuantity :: {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                            _addOrderLSEInstrumentID :: {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                            _addOrderReserved1 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                            _addOrderReserved2 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                            _addOrderPrice :: {-# UNPACK #-} !Data.ITCH.Types.Price,
                            _addOrderFlags :: {-# UNPACK #-} !Data.ITCH.Types.BitField}
                 | AddAttributedOrder{_addAttributedOrderNanosecond ::
                                      {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                      _addAttributedOrderOrderID ::
                                      {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                                      _addAttributedOrderSide ::
                                      {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                      _addAttributedOrderQuantity ::
                                      {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                      _addAttributedOrderLSEInstrumentID ::
                                      {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                      _addAttributedOrderReserved1 ::
                                      {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                      _addAttributedOrderReserved2 ::
                                      {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                      _addAttributedOrderPrice ::
                                      {-# UNPACK #-} !Data.ITCH.Types.Price,
                                      _addAttributedOrderAttribution ::
                                      {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                      _addAttributedOrderFlags ::
                                      {-# UNPACK #-} !Data.ITCH.Types.BitField}
                 | OrderDeleted{_orderDeletedNanosecond ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _orderDeletedOrderID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                                _orderDeletedFlags :: {-# UNPACK #-} !Data.ITCH.Types.BitField}
                 | OrderModified{_orderModifiedNanosecond ::
                                 {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                 _orderModifiedOrderID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                                 _orderModifiedNewQuantity ::
                                 {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                 _orderModifiedNewPrice :: {-# UNPACK #-} !Data.ITCH.Types.Price,
                                 _orderModifiedFlags :: {-# UNPACK #-} !Data.ITCH.Types.BitField}
                 | OrderBookClear{_orderBookClearNanosecond ::
                                  {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                  _orderBookClearLSEInstrumentID ::
                                  {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                  _orderBookClearReserved1 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                  _orderBookClearReserved2 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                  _orderBookClearFlags :: {-# UNPACK #-} !Data.ITCH.Types.BitField}
                 | Time{_timeSeconds :: {-# UNPACK #-} !Data.ITCH.Types.UInt32}
                 | LoginRequest{_loginRequestUsername ::
                                {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                _loginRequestPassword :: {-# UNPACK #-} !Data.ITCH.Types.Alpha}
                 | ReplayRequest{_replayRequestMarketDataGroup ::
                                 {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                 _replayRequestFirstMessage ::
                                 {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                 _replayRequestCount :: {-# UNPACK #-} !Data.ITCH.Types.UInt8}
                 | SnapshotRequest{_snapshotRequestSequenceNumber ::
                                   {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                   _snapshotRequestSegment :: {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                   _snapshotRequestLSEInstrumentID ::
                                   {-# UNPACK #-} !Data.ITCH.Types.UInt32}
                 | LogoutRequest{}
                 | LoginResponse{_loginResponseStatus ::
                                 {-# UNPACK #-} !Data.ITCH.Types.Byte}
                 | ReplayResponse{_replayResponseMarketDataGroup ::
                                  {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                  _replayResponseFirstMessage ::
                                  {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                  _replayResponseCount :: {-# UNPACK #-} !Data.ITCH.Types.UInt8,
                                  _replayResponseStatus :: {-# UNPACK #-} !Data.ITCH.Types.Byte}
                 | SnapshotResponse{_snapshotResponseSequenceNumber ::
                                    {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                    _snapshotResponseOrderCount ::
                                    {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                    _snapshotResponseStatus :: {-# UNPACK #-} !Data.ITCH.Types.Byte}
                 | SnapshotComplete{_snapshotCompleteSequenceNumber ::
                                    {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                    _snapshotCompleteSegment ::
                                    {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                    _snapshotCompleteLSEInstrumentID ::
                                    {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                    _snapshotCompleteFlags ::
                                    {-# UNPACK #-} !Data.ITCH.Types.BitField}
                 | SystemEvent{_systemEventNanosecond ::
                               {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                               _systemEventEventCode :: {-# UNPACK #-} !Data.ITCH.Types.Byte}
                 | SymbolDirectory{_symbolDirectoryNanosecond ::
                                   {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                   _symbolDirectoryLSEInstrumentID ::
                                   {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                   _symbolDirectoryReserved1 ::
                                   {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                   _symbolDirectoryReserved2 ::
                                   {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                   _symbolDirectorySymbolStatus ::
                                   {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                   _symbolDirectoryISIN :: {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                   _symbolDirectorySEDOL :: {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                   _symbolDirectorySegment :: {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                   _symbolDirectoryUnderlying ::
                                   {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                   _symbolDirectoryCurrency ::
                                   {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                   _symbolDirectoryTargetBook ::
                                   {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                   _symbolDirectorySecurityExchange ::
                                   {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                   _symbolDirectoryPreviousClosePrice ::
                                   {-# UNPACK #-} !Data.ITCH.Types.Price}
                 | SymbolStatus{_symbolStatusNanosecond ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _symbolStatusLSEInstrumentID ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _symbolStatusReserved1 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                _symbolStatusReserved2 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                _symbolStatusTradingStatus :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                _symbolStatusFlags :: {-# UNPACK #-} !Data.ITCH.Types.BitField,
                                _symbolStatusHaltReason :: {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                _symbolStatusSessionChangeReason ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt8,
                                _symbolStatusNewEndTime :: {-# UNPACK #-} !Data.ITCH.Types.Time,
                                _symbolStatusBookType :: {-# UNPACK #-} !Data.ITCH.Types.UInt8}
                 | OrderExecuted{_orderExecutedNanosecond ::
                                 {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                 _orderExecutedOrderID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                                 _orderExecutedExecutedQuantity ::
                                 {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                 _orderExecutedTradeID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64}
                 | OrderExecutedWithPrice{_orderExecutedWithPriceNanosecond ::
                                          {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                          _orderExecutedWithPriceOrderID ::
                                          {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                                          _orderExecutedWithPriceExecutedQuantity ::
                                          {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                          _orderExecutedWithPriceDisplayQuantity ::
                                          {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                          _orderExecutedWithPriceTradeID ::
                                          {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                                          _orderExecutedWithPricePrintable ::
                                          {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                          _orderExecutedWithPricePrice ::
                                          {-# UNPACK #-} !Data.ITCH.Types.Price}
                 | Trade{_tradeNanosecond :: {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                         _tradeExecutedQuantity :: {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                         _tradeLSEInstrumentID :: {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                         _tradeReserved1 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                         _tradeReserved2 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                         _tradePrice :: {-# UNPACK #-} !Data.ITCH.Types.Price,
                         _tradeTradeID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                         _tradeSideOfAggressor :: {-# UNPACK #-} !Data.ITCH.Types.Byte}
                 | AuctionTrade{_auctionTradeNanosecond ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _auctionTradeQuantity :: {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _auctionTradeLSEInstrumentID ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _auctionTradeReserved1 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                _auctionTradeReserved2 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                _auctionTradePrice :: {-# UNPACK #-} !Data.ITCH.Types.Price,
                                _auctionTradeTradeID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                                _auctionTradeAuctionType :: {-# UNPACK #-} !Data.ITCH.Types.Byte}
                 | TradeBreak{_tradeBreakNanosecond ::
                              {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                              _tradeBreakTradeID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                              _tradeBreakTradeType :: {-# UNPACK #-} !Data.ITCH.Types.Byte}
                 | AuctionInfo{_auctionInfoNanosecond ::
                               {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                               _auctionInfoPairedQuantity ::
                               {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                               _auctionInfoImbalanceQuantity ::
                               {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                               _auctionInfoImbalanceDirection ::
                               {-# UNPACK #-} !Data.ITCH.Types.Byte,
                               _auctionInfoLSEInstrumentID ::
                               {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                               _auctionInfoReserved1 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                               _auctionInfoReserved2 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                               _auctionInfoPrice :: {-# UNPACK #-} !Data.ITCH.Types.Price,
                               _auctionInfoAuctionType :: {-# UNPACK #-} !Data.ITCH.Types.Byte}
                 | OffBookTrade{_offBookTradeNanosecond ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _offBookTradeExecutedQuantity ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _offBookTradeLSEInstrumentID ::
                                {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                                _offBookTradeReserved1 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                _offBookTradeReserved2 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                                _offBookTradePrice :: {-# UNPACK #-} !Data.ITCH.Types.Price,
                                _offBookTradeTradeID :: {-# UNPACK #-} !Data.ITCH.Types.UInt64,
                                _offBookTradeOffBookTradeType ::
                                {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                _offBookTradeTradeTime :: {-# UNPACK #-} !Data.ITCH.Types.Time,
                                _offBookTradeTradeDate :: {-# UNPACK #-} !Data.ITCH.Types.Date,
                                _offBookTradeTradedCurrency ::
                                {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                _offBookTradeOriginalPrice ::
                                {-# UNPACK #-} !Data.ITCH.Types.Price,
                                _offBookTradeExecutionVenue ::
                                {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                                _offBookTradeFlags :: {-# UNPACK #-} !Data.ITCH.Types.BitField}
                 | Statistics{_statisticsNanosecond ::
                              {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                              _statisticsLSEInstrumentID ::
                              {-# UNPACK #-} !Data.ITCH.Types.UInt32,
                              _statisticsReserved1 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                              _statisticsReserved2 :: {-# UNPACK #-} !Data.ITCH.Types.Byte,
                              _statisticsLSEGStatisticsType ::
                              {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                              _statisticsPrice :: {-# UNPACK #-} !Data.ITCH.Types.Price,
                              _statisticsOpenClosePriceIndicator ::
                              {-# UNPACK #-} !Data.ITCH.Types.Alpha,
                              _statisticsFlags :: {-# UNPACK #-} !Data.ITCH.Types.BitField}
                 deriving (Show, Eq)
 
addOrder ::
         Data.ITCH.Types.UInt32 ->
           Data.ITCH.Types.UInt64 ->
             Data.ITCH.Types.Byte ->
               Data.ITCH.Types.UInt32 ->
                 Data.ITCH.Types.UInt32 ->
                   Data.ITCH.Types.Byte ->
                     Data.ITCH.Types.Byte ->
                       Data.ITCH.Types.Price -> Data.ITCH.Types.BitField -> ITCHMessage
addOrder = AddOrder
 
addAttributedOrder ::
                   Data.ITCH.Types.UInt32 ->
                     Data.ITCH.Types.UInt64 ->
                       Data.ITCH.Types.Byte ->
                         Data.ITCH.Types.UInt32 ->
                           Data.ITCH.Types.UInt32 ->
                             Data.ITCH.Types.Byte ->
                               Data.ITCH.Types.Byte ->
                                 Data.ITCH.Types.Price ->
                                   Data.ITCH.Types.Alpha -> Data.ITCH.Types.BitField -> ITCHMessage
addAttributedOrder = AddAttributedOrder
 
orderDeleted ::
             Data.ITCH.Types.UInt32 ->
               Data.ITCH.Types.UInt64 -> Data.ITCH.Types.BitField -> ITCHMessage
orderDeleted = OrderDeleted
 
orderModified ::
              Data.ITCH.Types.UInt32 ->
                Data.ITCH.Types.UInt64 ->
                  Data.ITCH.Types.UInt32 ->
                    Data.ITCH.Types.Price -> Data.ITCH.Types.BitField -> ITCHMessage
orderModified = OrderModified
 
orderBookClear ::
               Data.ITCH.Types.UInt32 ->
                 Data.ITCH.Types.UInt32 ->
                   Data.ITCH.Types.Byte ->
                     Data.ITCH.Types.Byte -> Data.ITCH.Types.BitField -> ITCHMessage
orderBookClear = OrderBookClear
 
time :: Data.ITCH.Types.UInt32 -> ITCHMessage
time = Time
 
loginRequest ::
             Data.ITCH.Types.Alpha -> Data.ITCH.Types.Alpha -> ITCHMessage
loginRequest = LoginRequest
 
replayRequest ::
              Data.ITCH.Types.Byte ->
                Data.ITCH.Types.UInt32 -> Data.ITCH.Types.UInt8 -> ITCHMessage
replayRequest = ReplayRequest
 
snapshotRequest ::
                Data.ITCH.Types.UInt32 ->
                  Data.ITCH.Types.Alpha -> Data.ITCH.Types.UInt32 -> ITCHMessage
snapshotRequest = SnapshotRequest
 
logoutRequest :: ITCHMessage
logoutRequest = LogoutRequest
 
loginResponse :: Data.ITCH.Types.Byte -> ITCHMessage
loginResponse = LoginResponse
 
replayResponse ::
               Data.ITCH.Types.Byte ->
                 Data.ITCH.Types.UInt32 ->
                   Data.ITCH.Types.UInt8 -> Data.ITCH.Types.Byte -> ITCHMessage
replayResponse = ReplayResponse
 
snapshotResponse ::
                 Data.ITCH.Types.UInt32 ->
                   Data.ITCH.Types.UInt32 -> Data.ITCH.Types.Byte -> ITCHMessage
snapshotResponse = SnapshotResponse
 
snapshotComplete ::
                 Data.ITCH.Types.UInt32 ->
                   Data.ITCH.Types.Alpha ->
                     Data.ITCH.Types.UInt32 -> Data.ITCH.Types.BitField -> ITCHMessage
snapshotComplete = SnapshotComplete
 
systemEvent ::
            Data.ITCH.Types.UInt32 -> Data.ITCH.Types.Byte -> ITCHMessage
systemEvent = SystemEvent
 
symbolDirectory ::
                Data.ITCH.Types.UInt32 ->
                  Data.ITCH.Types.UInt32 ->
                    Data.ITCH.Types.Byte ->
                      Data.ITCH.Types.Byte ->
                        Data.ITCH.Types.Alpha ->
                          Data.ITCH.Types.Alpha ->
                            Data.ITCH.Types.Alpha ->
                              Data.ITCH.Types.Alpha ->
                                Data.ITCH.Types.Alpha ->
                                  Data.ITCH.Types.Alpha ->
                                    Data.ITCH.Types.Byte ->
                                      Data.ITCH.Types.Alpha -> Data.ITCH.Types.Price -> ITCHMessage
symbolDirectory = SymbolDirectory
 
symbolStatus ::
             Data.ITCH.Types.UInt32 ->
               Data.ITCH.Types.UInt32 ->
                 Data.ITCH.Types.Byte ->
                   Data.ITCH.Types.Byte ->
                     Data.ITCH.Types.Byte ->
                       Data.ITCH.Types.BitField ->
                         Data.ITCH.Types.Alpha ->
                           Data.ITCH.Types.UInt8 ->
                             Data.ITCH.Types.Time -> Data.ITCH.Types.UInt8 -> ITCHMessage
symbolStatus = SymbolStatus
 
orderExecuted ::
              Data.ITCH.Types.UInt32 ->
                Data.ITCH.Types.UInt64 ->
                  Data.ITCH.Types.UInt32 -> Data.ITCH.Types.UInt64 -> ITCHMessage
orderExecuted = OrderExecuted
 
orderExecutedWithPrice ::
                       Data.ITCH.Types.UInt32 ->
                         Data.ITCH.Types.UInt64 ->
                           Data.ITCH.Types.UInt32 ->
                             Data.ITCH.Types.UInt32 ->
                               Data.ITCH.Types.UInt64 ->
                                 Data.ITCH.Types.Byte -> Data.ITCH.Types.Price -> ITCHMessage
orderExecutedWithPrice = OrderExecutedWithPrice
 
trade ::
      Data.ITCH.Types.UInt32 ->
        Data.ITCH.Types.UInt32 ->
          Data.ITCH.Types.UInt32 ->
            Data.ITCH.Types.Byte ->
              Data.ITCH.Types.Byte ->
                Data.ITCH.Types.Price ->
                  Data.ITCH.Types.UInt64 -> Data.ITCH.Types.Byte -> ITCHMessage
trade = Trade
 
auctionTrade ::
             Data.ITCH.Types.UInt32 ->
               Data.ITCH.Types.UInt32 ->
                 Data.ITCH.Types.UInt32 ->
                   Data.ITCH.Types.Byte ->
                     Data.ITCH.Types.Byte ->
                       Data.ITCH.Types.Price ->
                         Data.ITCH.Types.UInt64 -> Data.ITCH.Types.Byte -> ITCHMessage
auctionTrade = AuctionTrade
 
tradeBreak ::
           Data.ITCH.Types.UInt32 ->
             Data.ITCH.Types.UInt64 -> Data.ITCH.Types.Byte -> ITCHMessage
tradeBreak = TradeBreak
 
auctionInfo ::
            Data.ITCH.Types.UInt32 ->
              Data.ITCH.Types.UInt32 ->
                Data.ITCH.Types.UInt32 ->
                  Data.ITCH.Types.Byte ->
                    Data.ITCH.Types.UInt32 ->
                      Data.ITCH.Types.Byte ->
                        Data.ITCH.Types.Byte ->
                          Data.ITCH.Types.Price -> Data.ITCH.Types.Byte -> ITCHMessage
auctionInfo = AuctionInfo
 
offBookTrade ::
             Data.ITCH.Types.UInt32 ->
               Data.ITCH.Types.UInt32 ->
                 Data.ITCH.Types.UInt32 ->
                   Data.ITCH.Types.Byte ->
                     Data.ITCH.Types.Byte ->
                       Data.ITCH.Types.Price ->
                         Data.ITCH.Types.UInt64 ->
                           Data.ITCH.Types.Alpha ->
                             Data.ITCH.Types.Time ->
                               Data.ITCH.Types.Date ->
                                 Data.ITCH.Types.Alpha ->
                                   Data.ITCH.Types.Price ->
                                     Data.ITCH.Types.Alpha ->
                                       Data.ITCH.Types.BitField -> ITCHMessage
offBookTrade = OffBookTrade
 
statistics ::
           Data.ITCH.Types.UInt32 ->
             Data.ITCH.Types.UInt32 ->
               Data.ITCH.Types.Byte ->
                 Data.ITCH.Types.Byte ->
                   Data.ITCH.Types.Alpha ->
                     Data.ITCH.Types.Price ->
                       Data.ITCH.Types.Alpha -> Data.ITCH.Types.BitField -> ITCHMessage
statistics = Statistics
 
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
         (Data.ITCH.Types.arbitraryAlpha 11))
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
  = (<*>) ((<$>) LoginRequest (Data.ITCH.Types.arbitraryAlpha 6))
      (Data.ITCH.Types.arbitraryAlpha 10)
 
arbitraryReplayRequest :: Gen ITCHMessage
arbitraryReplayRequest
  = (<*>) ((<*>) ((<$>) ReplayRequest arbitrary) arbitrary) arbitrary
 
arbitrarySnapshotRequest :: Gen ITCHMessage
arbitrarySnapshotRequest
  = (<*>)
      ((<*>) ((<$>) SnapshotRequest arbitrary)
         (Data.ITCH.Types.arbitraryAlpha 6))
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
      ((<*>)
         ((<*>) ((<$>) SnapshotComplete arbitrary)
            (Data.ITCH.Types.arbitraryAlpha 6))
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
                              (Data.ITCH.Types.arbitraryAlpha 1))
                           (Data.ITCH.Types.arbitraryAlpha 12))
                        (Data.ITCH.Types.arbitraryAlpha 12))
                     (Data.ITCH.Types.arbitraryAlpha 6))
                  (Data.ITCH.Types.arbitraryAlpha 6))
               (Data.ITCH.Types.arbitraryAlpha 3))
            arbitrary)
         (Data.ITCH.Types.arbitraryAlpha 4))
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
               (Data.ITCH.Types.arbitraryAlpha 4))
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
                        (Data.ITCH.Types.arbitraryAlpha 4))
                     arbitrary)
                  arbitrary)
               (Data.ITCH.Types.arbitraryAlpha 4))
            arbitrary)
         (Data.ITCH.Types.arbitraryAlpha 5))
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
               (Data.ITCH.Types.arbitraryAlpha 1))
            arbitrary)
         (Data.ITCH.Types.arbitraryAlpha 1))
      arbitrary