thebook-haskell - Electronic order book in Haskell.
===================================================

[![Build Status](https://travis-ci.org/jkozlowski/thebook-haskell.png?branch=master)](https://travis-ci.org/jkozlowski/thebook-haskell)
[![Coverage Status](http://coveralls.io/repos/jkozlowski/thebook-haskell/badge.png?branch=master)](http://coveralls.io/r/jkozlowski/thebook-haskell?branch=master)

The overriding goal of this project is to (eventually) create a (near) clone of the [LSE SETS](http://www.londonstockexchange.com/products-and-services/trading-services/sets/sets.htm) system. The project is progressing slowly but hopefully some day it will actually be useable.

## Done

* Parser of ITCH.xml schemas that generates the records,
  binary and arbitrary instances for the messages (almost as part of the build).
* Stupid conduit demo where server pushes random 
  ITCH messages and client prints those.

## Goals and TODOs

### General features
* Networking (FIX in/OUCH or ITCH out)
* Separate business logic (model of order book) from the side-effecting bits.
* Separate market data distribution engine.
* Admin commands (create market, clear order book)

### Order types
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

### Config
* Use [dire](http://hackage.haskell.org/package/dyre) for config.

### Examples

* Use [doctest](https://github.com/sol/doctest-haskell#readme) for examples in haddocks.

## Phases
* Validation (NewOrderSingle enters -> Particular order comes out)
* The following are performed until trade happens
* Depending on the order a particular rule is invoked for matching it and that results in TradeInstruction
* This then goes to the order book to be executed and results in Trade
* Then we invoke rules on the trade (if e.g. iceberg is executed, it needs to reenter book)
* Once no more trades can happen, we go through rules on what to do with order next (Market is cancelled, limit order enters book etc.)

## Useful
* http://www.londonstockexchange.com/products-and-services/millennium-exchange/technicalinformation/technicalinformation.htm
* http://www.londonstockexchange.com/products-and-services/millennium-exchange/millennium-exchange-migration/mit203-issue103final.pdf
* http://www.londonstockexchange.com/products-and-services/millennium-exchange/millennium-exchange-migration/mit303.pdf

### ITCH
* http://www.onixs.biz/lse-level-2-itch-market-data-handler.html
