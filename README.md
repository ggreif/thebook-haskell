thebook-haskell
===============

Exchange simulator in Haskell

## TODO
* Limit orders
* Market orders
* TimeInForce
* Stop limit orders
* Stop orders
* Iceberg orders
* Passive Only
* Hidden Limit Orders
* Mid Price Pegged Orders
* Named Orders
* Executable Quotes
* Firm Quotes

## Phases
* Validation (NewOrderSingle enters -> Particular order comes out)
* The following are performed until trade happens
* Depending on the order a particular rule is invoked for matching it and that results in TradeInstruction
* This then goes to the order book to be executed and results in Trade
* Then we invoke rules on the trade (if e.g. iceberg is executed, it needs to reenter book)
* Once no more trades can happen, we go through rules on what to do with order next (Market is cancelled, limit order enters book etc.)

## Useful
* http://www.londonstockexchange.com/products-and-services/millennium-exchange/technicalinformation/technicalinformation.htm
