module Data.ITCH.ITCH51 (ITCHMessage(..)) where
import Data.ITCH.Types
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
         get)
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
getLoginRequest = (<*>) ((<$>) LoginRequest get) get
 
getReplayRequest :: Get ITCHMessage
getReplayRequest = (<*>) ((<*>) ((<$>) ReplayRequest get) get) get
 
getSnapshotRequest :: Get ITCHMessage
getSnapshotRequest
  = (<*>) ((<*>) ((<$>) SnapshotRequest get) get) get
 
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
  = (<*>) ((<*>) ((<*>) ((<$>) SnapshotComplete get) get) get) get
 
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
                              get)
                           get)
                        get)
                     get)
                  get)
               get)
            get)
         get)
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
               get)
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
                        get)
                     get)
                  get)
               get)
            get)
         get)
      get
 
getStatistics :: Get ITCHMessage
getStatistics
  = (<*>)
      ((<*>)
         ((<*>)
            ((<*>) ((<*>) ((<*>) ((<*>) ((<$>) Statistics get) get) get) get)
               get)
            get)
         get)
      get
 
instance Binary ITCHMessage where
        get
          = do msgType <- getMessageType
               case msgType of
                   65 -> getAddOrder
                   70 -> getAddAttributedOrder
                   68 -> getOrderDeleted
                   85 -> getOrderModified
                   121 -> getOrderBookClear
                   84 -> getTime
                   1 -> getLoginRequest
                   3 -> getReplayRequest
                   129 -> getSnapshotRequest
                   5 -> getLogoutRequest
                   2 -> getLoginResponse
                   4 -> getReplayResponse
                   130 -> getSnapshotResponse
                   131 -> getSnapshotComplete
                   83 -> getSystemEvent
                   82 -> getSymbolDirectory
                   72 -> getSymbolStatus
                   69 -> getOrderExecuted
                   67 -> getOrderExecutedWithPrice
                   80 -> getTrade
                   81 -> getAuctionTrade
                   66 -> getTradeBreak
                   73 -> getAuctionInfo
                   120 -> getOffBookTrade
                   119 -> getStatistics
                   _ -> fail "Unknown msg type"
        put msg@AddOrder{}
          = (*>) (putMessageType 65)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>) ((*>) (put (_addOrderFlags msg)) (put (_addOrderPrice msg)))
                                   (put (_addOrderReserved2 msg)))
                                (put (_addOrderReserved1 msg)))
                             (put (_addOrderLSEInstrumentID msg)))
                          (put (_addOrderQuantity msg)))
                       (put (_addOrderSide msg)))
                    (put (_addOrderOrderID msg)))
                 (put (_addOrderNanosecond msg)))
        put msg@AddAttributedOrder{}
          = (*>) (putMessageType 70)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>)
                                      ((*>) (put (_addAttributedOrderFlags msg))
                                         (put (_addAttributedOrderAttribution msg)))
                                      (put (_addAttributedOrderPrice msg)))
                                   (put (_addAttributedOrderReserved2 msg)))
                                (put (_addAttributedOrderReserved1 msg)))
                             (put (_addAttributedOrderLSEInstrumentID msg)))
                          (put (_addAttributedOrderQuantity msg)))
                       (put (_addAttributedOrderSide msg)))
                    (put (_addAttributedOrderOrderID msg)))
                 (put (_addAttributedOrderNanosecond msg)))
        put msg@OrderDeleted{}
          = (*>) (putMessageType 68)
              ((*>)
                 ((*>) (put (_orderDeletedFlags msg))
                    (put (_orderDeletedOrderID msg)))
                 (put (_orderDeletedNanosecond msg)))
        put msg@OrderModified{}
          = (*>) (putMessageType 85)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>) (put (_orderModifiedFlags msg))
                          (put (_orderModifiedNewPrice msg)))
                       (put (_orderModifiedNewQuantity msg)))
                    (put (_orderModifiedOrderID msg)))
                 (put (_orderModifiedNanosecond msg)))
        put msg@OrderBookClear{}
          = (*>) (putMessageType 121)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>) (put (_orderBookClearFlags msg))
                          (put (_orderBookClearReserved2 msg)))
                       (put (_orderBookClearReserved1 msg)))
                    (put (_orderBookClearLSEInstrumentID msg)))
                 (put (_orderBookClearNanosecond msg)))
        put msg@Time{} = (*>) (putMessageType 84) (put (_timeSeconds msg))
        put msg@LoginRequest{}
          = (*>) (putMessageType 1)
              ((*>) (put (_loginRequestPassword msg))
                 (put (_loginRequestUsername msg)))
        put msg@ReplayRequest{}
          = (*>) (putMessageType 3)
              ((*>)
                 ((*>) (put (_replayRequestCount msg))
                    (put (_replayRequestFirstMessage msg)))
                 (put (_replayRequestMarketDataGroup msg)))
        put msg@SnapshotRequest{}
          = (*>) (putMessageType 129)
              ((*>)
                 ((*>) (put (_snapshotRequestLSEInstrumentID msg))
                    (put (_snapshotRequestSegment msg)))
                 (put (_snapshotRequestSequenceNumber msg)))
        put msg@LogoutRequest{} = (*>) (putMessageType 5) (return ())
        put msg@LoginResponse{}
          = (*>) (putMessageType 2) (put (_loginResponseStatus msg))
        put msg@ReplayResponse{}
          = (*>) (putMessageType 4)
              ((*>)
                 ((*>)
                    ((*>) (put (_replayResponseStatus msg))
                       (put (_replayResponseCount msg)))
                    (put (_replayResponseFirstMessage msg)))
                 (put (_replayResponseMarketDataGroup msg)))
        put msg@SnapshotResponse{}
          = (*>) (putMessageType 130)
              ((*>)
                 ((*>) (put (_snapshotResponseStatus msg))
                    (put (_snapshotResponseOrderCount msg)))
                 (put (_snapshotResponseSequenceNumber msg)))
        put msg@SnapshotComplete{}
          = (*>) (putMessageType 131)
              ((*>)
                 ((*>)
                    ((*>) (put (_snapshotCompleteFlags msg))
                       (put (_snapshotCompleteLSEInstrumentID msg)))
                    (put (_snapshotCompleteSegment msg)))
                 (put (_snapshotCompleteSequenceNumber msg)))
        put msg@SystemEvent{}
          = (*>) (putMessageType 83)
              ((*>) (put (_systemEventEventCode msg))
                 (put (_systemEventNanosecond msg)))
        put msg@SymbolDirectory{}
          = (*>) (putMessageType 82)
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
                                               ((*>) (put (_symbolDirectoryPreviousClosePrice msg))
                                                  (put (_symbolDirectorySecurityExchange msg)))
                                               (put (_symbolDirectoryTargetBook msg)))
                                            (put (_symbolDirectoryCurrency msg)))
                                         (put (_symbolDirectoryUnderlying msg)))
                                      (put (_symbolDirectorySegment msg)))
                                   (put (_symbolDirectorySEDOL msg)))
                                (put (_symbolDirectoryISIN msg)))
                             (put (_symbolDirectorySymbolStatus msg)))
                          (put (_symbolDirectoryReserved2 msg)))
                       (put (_symbolDirectoryReserved1 msg)))
                    (put (_symbolDirectoryLSEInstrumentID msg)))
                 (put (_symbolDirectoryNanosecond msg)))
        put msg@SymbolStatus{}
          = (*>) (putMessageType 72)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>)
                                      ((*>) (put (_symbolStatusBookType msg))
                                         (put (_symbolStatusNewEndTime msg)))
                                      (put (_symbolStatusSessionChangeReason msg)))
                                   (put (_symbolStatusHaltReason msg)))
                                (put (_symbolStatusFlags msg)))
                             (put (_symbolStatusTradingStatus msg)))
                          (put (_symbolStatusReserved2 msg)))
                       (put (_symbolStatusReserved1 msg)))
                    (put (_symbolStatusLSEInstrumentID msg)))
                 (put (_symbolStatusNanosecond msg)))
        put msg@OrderExecuted{}
          = (*>) (putMessageType 69)
              ((*>)
                 ((*>)
                    ((*>) (put (_orderExecutedTradeID msg))
                       (put (_orderExecutedExecutedQuantity msg)))
                    (put (_orderExecutedOrderID msg)))
                 (put (_orderExecutedNanosecond msg)))
        put msg@OrderExecutedWithPrice{}
          = (*>) (putMessageType 67)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>) (put (_orderExecutedWithPricePrice msg))
                                (put (_orderExecutedWithPricePrintable msg)))
                             (put (_orderExecutedWithPriceTradeID msg)))
                          (put (_orderExecutedWithPriceDisplayQuantity msg)))
                       (put (_orderExecutedWithPriceExecutedQuantity msg)))
                    (put (_orderExecutedWithPriceOrderID msg)))
                 (put (_orderExecutedWithPriceNanosecond msg)))
        put msg@Trade{}
          = (*>) (putMessageType 80)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>) (put (_tradeSideOfAggressor msg)) (put (_tradeTradeID msg)))
                                (put (_tradePrice msg)))
                             (put (_tradeReserved2 msg)))
                          (put (_tradeReserved1 msg)))
                       (put (_tradeLSEInstrumentID msg)))
                    (put (_tradeExecutedQuantity msg)))
                 (put (_tradeNanosecond msg)))
        put msg@AuctionTrade{}
          = (*>) (putMessageType 81)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>) (put (_auctionTradeAuctionType msg))
                                   (put (_auctionTradeTradeID msg)))
                                (put (_auctionTradePrice msg)))
                             (put (_auctionTradeReserved2 msg)))
                          (put (_auctionTradeReserved1 msg)))
                       (put (_auctionTradeLSEInstrumentID msg)))
                    (put (_auctionTradeQuantity msg)))
                 (put (_auctionTradeNanosecond msg)))
        put msg@TradeBreak{}
          = (*>) (putMessageType 66)
              ((*>)
                 ((*>) (put (_tradeBreakTradeType msg))
                    (put (_tradeBreakTradeID msg)))
                 (put (_tradeBreakNanosecond msg)))
        put msg@AuctionInfo{}
          = (*>) (putMessageType 73)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>)
                                   ((*>) (put (_auctionInfoAuctionType msg))
                                      (put (_auctionInfoPrice msg)))
                                   (put (_auctionInfoReserved2 msg)))
                                (put (_auctionInfoReserved1 msg)))
                             (put (_auctionInfoLSEInstrumentID msg)))
                          (put (_auctionInfoImbalanceDirection msg)))
                       (put (_auctionInfoImbalanceQuantity msg)))
                    (put (_auctionInfoPairedQuantity msg)))
                 (put (_auctionInfoNanosecond msg)))
        put msg@OffBookTrade{}
          = (*>) (putMessageType 120)
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
                                                  ((*>) (put (_offBookTradeFlags msg))
                                                     (put (_offBookTradeExecutionVenue msg)))
                                                  (put (_offBookTradeOriginalPrice msg)))
                                               (put (_offBookTradeTradedCurrency msg)))
                                            (put (_offBookTradeTradeDate msg)))
                                         (put (_offBookTradeTradeTime msg)))
                                      (put (_offBookTradeOffBookTradeType msg)))
                                   (put (_offBookTradeTradeID msg)))
                                (put (_offBookTradePrice msg)))
                             (put (_offBookTradeReserved2 msg)))
                          (put (_offBookTradeReserved1 msg)))
                       (put (_offBookTradeLSEInstrumentID msg)))
                    (put (_offBookTradeExecutedQuantity msg)))
                 (put (_offBookTradeNanosecond msg)))
        put msg@Statistics{}
          = (*>) (putMessageType 119)
              ((*>)
                 ((*>)
                    ((*>)
                       ((*>)
                          ((*>)
                             ((*>)
                                ((*>) (put (_statisticsFlags msg))
                                   (put (_statisticsOpenClosePriceIndicator msg)))
                                (put (_statisticsPrice msg)))
                             (put (_statisticsLSEGStatisticsType msg)))
                          (put (_statisticsReserved2 msg)))
                       (put (_statisticsReserved1 msg)))
                    (put (_statisticsLSEInstrumentID msg)))
                 (put (_statisticsNanosecond msg)))
        put _ = fail "Unknown msg type"

addOrder ::
         UInt32 ->
           UInt64 ->
             Byte ->
               UInt32 ->
                 UInt32 -> Byte -> Byte -> Price -> BitField -> ITCHMessage
addOrder = AddOrder
 
addAttributedOrder ::
                   UInt32 ->
                     UInt64 ->
                       Byte ->
                         UInt32 ->
                           UInt32 -> Byte -> Byte -> Price -> Alpha -> BitField -> ITCHMessage
addAttributedOrder = AddAttributedOrder
 
orderDeleted :: UInt32 -> UInt64 -> BitField -> ITCHMessage
orderDeleted = OrderDeleted
 
orderModified ::
              UInt32 -> UInt64 -> UInt32 -> Price -> BitField -> ITCHMessage
orderModified = OrderModified
 
orderBookClear ::
               UInt32 -> UInt32 -> Byte -> Byte -> BitField -> ITCHMessage
orderBookClear = OrderBookClear
 
time :: UInt32 -> ITCHMessage
time = Time
 
loginRequest :: Alpha -> Alpha -> ITCHMessage
loginRequest = LoginRequest
 
replayRequest :: Byte -> UInt32 -> UInt8 -> ITCHMessage
replayRequest = ReplayRequest
 
snapshotRequest :: UInt32 -> Alpha -> UInt32 -> ITCHMessage
snapshotRequest = SnapshotRequest
 
logoutRequest :: ITCHMessage
logoutRequest = LogoutRequest
 
loginResponse :: Byte -> ITCHMessage
loginResponse = LoginResponse
 
replayResponse :: Byte -> UInt32 -> UInt8 -> Byte -> ITCHMessage
replayResponse = ReplayResponse
 
snapshotResponse :: UInt32 -> UInt32 -> Byte -> ITCHMessage
snapshotResponse = SnapshotResponse
 
snapshotComplete ::
                 UInt32 -> Alpha -> UInt32 -> BitField -> ITCHMessage
snapshotComplete = SnapshotComplete
 
systemEvent :: UInt32 -> Byte -> ITCHMessage
systemEvent = SystemEvent
 
symbolDirectory ::
                UInt32 ->
                  UInt32 ->
                    Byte ->
                      Byte ->
                        Alpha ->
                          Alpha ->
                            Alpha ->
                              Alpha -> Alpha -> Alpha -> Byte -> Alpha -> Price -> ITCHMessage
symbolDirectory = SymbolDirectory
 
symbolStatus ::
             UInt32 ->
               UInt32 ->
                 Byte ->
                   Byte ->
                     Byte -> BitField -> Alpha -> UInt8 -> Time -> UInt8 -> ITCHMessage
symbolStatus = SymbolStatus
 
orderExecuted ::
              UInt32 -> UInt64 -> UInt32 -> UInt64 -> ITCHMessage
orderExecuted = OrderExecuted
 
orderExecutedWithPrice ::
                       UInt32 ->
                         UInt64 ->
                           UInt32 -> UInt32 -> UInt64 -> Byte -> Price -> ITCHMessage
orderExecutedWithPrice = OrderExecutedWithPrice
 
trade ::
      UInt32 ->
        UInt32 ->
          UInt32 -> Byte -> Byte -> Price -> UInt64 -> Byte -> ITCHMessage
trade = Trade
 
auctionTrade ::
             UInt32 ->
               UInt32 ->
                 UInt32 -> Byte -> Byte -> Price -> UInt64 -> Byte -> ITCHMessage
auctionTrade = AuctionTrade
 
tradeBreak :: UInt32 -> UInt64 -> Byte -> ITCHMessage
tradeBreak = TradeBreak
 
auctionInfo ::
            UInt32 ->
              UInt32 ->
                UInt32 ->
                  Byte -> UInt32 -> Byte -> Byte -> Price -> Byte -> ITCHMessage
auctionInfo = AuctionInfo
 
offBookTrade ::
             UInt32 ->
               UInt32 ->
                 UInt32 ->
                   Byte ->
                     Byte ->
                       Price ->
                         UInt64 ->
                           Alpha ->
                             Time -> Date -> Alpha -> Price -> Alpha -> BitField -> ITCHMessage
offBookTrade = OffBookTrade
 
statistics ::
           UInt32 ->
             UInt32 ->
               Byte -> Byte -> Alpha -> Price -> Alpha -> BitField -> ITCHMessage
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