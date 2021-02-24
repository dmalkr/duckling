{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfCryptocurrency.Corpus
  ( corpus
  ) where


import Data.String
import Prelude

import Duckling.AmountOfCryptocurrency.AmountOfCryptocurrency
import Duckling.Resolve (Options(..))
import Duckling.Testing.Types


corpus :: Corpus
corpus = (testContext, testOptions, allExamples)


simple :: Cryptocurrency -> Double -> AmountOfCryptocurrencyValue
simple cur v = AmountOfCryptocurrencyValue (Just v) cur


onlyCurrency :: Cryptocurrency -> AmountOfCryptocurrencyValue
onlyCurrency cur = AmountOfCryptocurrencyValue Nothing cur


allExamples :: [Example]
allExamples = concat
  [ examples (onlyCurrency BTC)
             [ "bitcoin"
             , "bitcoins"
             , "btc"
             , "BTC"
             ]
  , examples (onlyCurrency ETC)
             [ "ETC"
             , "ethereum classic"
             , "etcusd"
             , "etc usd"
             ]
  , examples (simple BTC 10.0)
             [ "BTC 10"
             , "10 BTC"
             , "ten bitcoins"
             , "ten btcusd"
             , "ten btc usd"
             ]
  , examples (simple BTC 15.678)
             [ "BTC 15.678"
             , "15.678 BTC"
             , "15.678 Bitcoins"
             , "15.678 BTCUSD"
             , "15.678 bitcoin us dollars"
             ]
  , examples (simple XAI 32.54)
             [ "xaiusd 32.54"
             , "32.54 xaiusd"
             , "32.54 xai"
             , "32.54 xai usd"
             , "32.54 altcoins"
             , "32.54 altcoin index"
             ]
  , examples (simple BAT 12.3456)
             [ "BAT 12.3456"
             , "12.3456 bat"
             , "12.3456 batusd"
             , "12.3456 bat usd"
             ]
  , examples (simple BCH 13.3456)
             [ "BCH 13.3456"
             , "13.3456 bch"
             , "13.3456 bchusd"
             , "13.3456 bch usd"
             , "13.3456 bitcoin cash"
             , "13.3456 bitcoincash"
             ]
  , examples (simple DASH 14.3456)
             [ "DASH 14.3456"
             , "14.3456 dash"
             , "14.3456 darkcoin"
             , "Darkcoin 14.3456"
             ]
  , examples (simple EOS 15.3456)
             [ "EOS 15.3456"
             , "15.3456 eos"
             , "15.3456 eos io"
             , "eos io 15.3456"
             , "eos.io 15.3456"
             , "15.3456 eos.io"
             , "eosio 15.3456"
             , "15.3456 eosio"
             , "eosusd 15.3456"
             , "15.3456 eosusd"
             ]
  , examples (simple ETH 16.3456)
             [ "ETH 16.3456"
             , "16.3456 ETH"
             , "16.3456 ethereum"
             , "ethereum 16.3456"
             , "16.3456 ether"
             , "ether 16.3456"
             , "16.3456 etherium"
             , "etherium 16.3456"
             , "16.3456 ethusd"
             , "ethusd 16.3456"
             , "16.3456 eth usd"
             , "eth usd 16.3456"
             ]
  , examples (simple ETC 17.3456)
             [ "ETC 17.3456"
             , "17.3456 ETC"
             , "17.3456 ethereum classic"
             , "ethereum classic 17.3456"
             , "17.3456 etherium classic"
             , "etherium classic 17.3456"
             , "17.3456 etcusd"
             , "etcusd 17.3456"
             , "17.3456 etc usd"
             , "etc usd 17.3456"
             ]
  , examples (simple IOTA 18.3456)
             [ "iot 18.3456"
             , "18.3456 IOT"
             , "iota 18.3456"
             , "18.3456 IOTA"
             , "iotusd 18.3456"
             , "18.3456 iotusd"
             , "iot usd 18.3456"
             , "18.3456 iot usd"
             ]
  , examples (simple LTC 19.3456)
             [ "LTC 19.3456"
             , "19.3456 LTC"
             , "litecoin 19.3456"
             , "19.3456 litecoin"
             , "ltcusd 19.3456"
             , "19.3456 ltcusd"
             , "ltc usd 19.3456"
             , "19.3456 ltc usd"
             , "Ł 19.3456"
             , "19.3456 Ł"
             ]
  , examples (simple XMR 20.3456)
             [ "XMR 20.3456"
             , "20.3456 xmr"
             , "monero 20.3456"
             , "20.3456 monero"
             , "xmrusd 20.3456"
             , "20.3456 xmrusd"
             , "xmr usd 20.3456"
             , "20.3456 xmr usd"
             ]
  , examples (simple NEO 21.3456)
             [ "NEO 21.3456"
             , "21.3456 neo"
             , "NEOUSD 21.3456"
             , "21.3456 neousd"
             , "neo usd 21.3456"
             , "21.3456 neo usd"
             ]
  , examples (simple OMG 22.3456)
             [ "OMG 22.3456"
             , "22.3456 omg"
             , "OMGUSD 22.3456"
             , "22.3456 omgusd"
             , "omg usd 22.3456"
             , "22.3456 omg usd"
             , "omisego 22.3456"
             , "22.3456 OmiseGo"
             ]
  , examples (simple XRP 23.3456)
             [ "xrp 23.3456"
             , "23.3456 xrp"
             , "XRPUSD 23.3456"
             , "23.3456 xrpusd"
             , "xrp usd 23.3456"
             , "23.3456 xrp usd"
             , "ripple 23.3456"
             , "23.3456 ripple"
             , "ripl 23.3456"
             , "23.3456 ripl"
             , "riple 23.3456"
             , "23.3456 riple"
             ]
  , examples (simple XLM 24.3456)
             [ "xlm 24.3456"
             , "24.3456 xlm"
             , "XLMUSD 24.3456"
             , "24.3456 XLMUSD"
             , "xlm usd 24.3456"
             , "24.3456 xlm usd"
             , "stellar 24.3456"
             , "24.3456 stellar"
             , "stelar 24.3456"
             , "24.3456 stelar"
             , "stellar lumen 24.3456"
             , "24.3456 stellar lumen"
             , "stelar lumen 24.3456"
             , "24.3456 stelar lumen"
             ]
  , examples (simple ZEC 25.3456)
             [ "ZCASH 25.3456"
             , "25.3456 ZCASH"
             , "ZECUSD 25.3456"
             , "25.3456 ZECUSD"
             , "zec usd 25.3456"
             , "25.3456 zec usd"
             ]
  ]
