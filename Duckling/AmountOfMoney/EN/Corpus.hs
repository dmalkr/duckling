-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.EN.Corpus
  ( corpus
  , negativeCorpus
  , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Resolve (Options(..))
import Duckling.Testing.Types

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "exactly dollars"
      ]

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

latentCorpus :: Corpus
latentCorpus = (testContext, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (simple Unnamed 5)
                 [ "five"
                 , "5"
                 , "about 5"
                 ]
      , examples (simple Unnamed 7.2)
                 [ "7.2"
                 , "7.20000"
                 ]
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "one dollar"
             , "a dollar"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 dollars"
             , "ten dollars"
             ]
  , examples (simple Cent 10)
             [ "10 cent"
             , "ten pennies"
             , "ten cents"
             , "10 c"
             , "10¢"
             ]
  , examples (simple Dollar 1e4)
             [ "$10K"
             , "10k$"
             , "$10,000"
             ]
  , examples (simple USD 3.14)
             [ "USD3.14"
             , "3.14US$"
             , "US$ 3.14"
             , "US$3 and fourteen"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             , "EUR 20.0"
             , "20€"
             , "20 €ur"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "ten pounds"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple INR 20.43)
             [ "20 Rupees 43"
             , "twenty rupees 43"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 and 43c"
             , "$20 43"
             , "20 dollar 43c"
             , "20 dollars 43 cents"
             , "twenty dollar 43 cents"
             , "20 dollar 43"
             , "twenty dollar and 43"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 GBP 1 pence"
             , "3 GBP and one"
             ]
  , examples (simple CAD 3.03)
             [ "CAD3.03"
             , "CAD 3.03"
             , "3 CAD 3 cents"
             ]
  , examples (simple CHF 3.04)
             [ "CHF3.04"
             , "CHF 3.04"
             , "3 CHF 4 cents"
             ]
  , examples (simple CNY 3)
             [ "CNY3"
             , "CNY 3"
             , "3 CNY"
             , "3 yuan"
             ]
  , examples (simple Unnamed 42)
             [ "42 bucks"
             , "around 42 bucks"
             , "exactly 42 bucks"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             , "42 kuwaiti Dinar"
             ]
  , examples (simple LBP 42)
             [ "42 LBP"
             , "42 Lebanese Pounds"
             ]
  , examples (simple EGP 42)
             [ "42 EGP"
             , "42 egyptianpound"
             ]
  , examples (simple QAR 42)
             [ "42 QAR"
             , "42 qatari riyals"
             ]
  , examples (simple SAR 42)
             [ "42 SAR"
             , "42 Saudiriyal"
             ]
  , examples (simple BGN 42)
             [ "42 BGN"
             ]
  , examples (simple MYR 42)
             [ "42 MYR"
             , "42 RM"
             , "RM 42"
             , "MYR 42"
             , "42MYR"
             , "42RM"
             , "RM42"
             , "MYR42"
             , "ringgit 42"
             , "42 ringgit"
             , "42 malaysia ringgit"
             , "malaysia ringgit 42"
             , "42 malaysian ringgit"
             , "malaysian ringgit 42"
             , "42 malaysia ringgits"
             , "malaysia ringgits 42"
             , "42 malaysian ringgits"
             , "malaysian ringgits 42"
             ]
  , examples (simple MYR 20.43)
             [ "20 ringgit and 43c"
             , "20 ringgit and 43 sen"
             , "twenty ringgit 43 sens"
             , "20 ringgit 43"
             , "twenty ringgit and 43"
             ]
  , examples (simple Dinar 10)
             [ "10 dinars"
             ]
  , examples (simple ILS 10)
             [ "ten shekels"
             , "10 ILS"
             ]
  , examples (simple Riyal 10)
             [ "ten riyals"
             , "10 riyals"
             ]
  , examples (simple Rial 10)
             [ "ten rials"
             , "10 rials"
             ]
  , examples (between Dollar (10, 20))
             [ "between 10 and 20 dollars"
             , "from 10 dollars to 20 dollars"
             , "around 10-20 dollars"
             , "between 10 dollars and 20 dollars"
             , "from 10 to 20 dollars"
             , "about $10-$20"
             , "10-20 dollars"
             ]
  , examples (between Dollar (1.1, 1.3))
             [ "between 1.1 and 1.3 dollars"
             , "from 1 point 1 and one point three dollars"
             ]
  , examples (under EUR 7)
             [ "under seven euros"
             , "less than 7 EUR"
             , "lower than 7€"
             , "no more than 7 euros"
             , "at most 7€"
             ]
  , examples (above Dollar 1.42)
             [ "more than 1 dollar and forty-two cents"
             , "at least $1.42"
             , "over 1.42 dollars"
             , "above a dollar and 42 cents"
             , "no less than $1.42"
             ]
   , examples (simple INR 5e5)
             [ "5 lakh rupees"
             , "five lakhs rupees"
             ]
   , examples (between INR (7, 9e5))
             [ "7-9 lakh rupees"
             ]
   , examples (simple INR 4e7)
             [ "four crore rupees"
             , "4 crores rupees"
             ]
   , examples (simple MNT 10)
             [ "ten tugriks"
             , "10 Tugrik"
             , "10MNT"
             ]
   , examples (simple USD 4.7e9)
             [ "US$4.7 billion"
             , "a US$4.7 billion"
             , "a US$ 4.7 billion"
             ]
  , examples (simple UAH 3.04)
             [ "UAH3.04"
             , "UAH 3.04"
             , "3 UAH 4 kopiykas"
             ]
  -- Cryptocurrencies
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
