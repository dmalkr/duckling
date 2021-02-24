-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Hashable
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Prelude
import qualified Data.HashSet as HashSet
import qualified TextShow as TS
import qualified TextShow.Generic as TS
import Text.Pretty.Simple

import Duckling.Debug
import Duckling.Locale
import Duckling.Numeral.Helpers (isPositive)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types (GroupMatch(..))
import Duckling.Resolve (Resolve(..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral


data Cryptocurrency
  = BTC
  | XAI
  | BAT
  | BCH
  | DASH
  | EOS
  | ETH
  | ETC
  | IOTA
  | LTC
  | XMR
  | NEO
  | OMG
  | XRP
  | XLM
  | ZEC
  deriving (Eq, Generic, Hashable, Show, Ord, NFData, ToJSON)
  deriving TS.TextShow via TS.FromGeneric Cryptocurrency


cryptocurrencies :: [(Cryptocurrency, String)]
cryptocurrencies =
  [ (BTC,  "bitcoins?( us dollars?)?|btc( ?usd)?")
  , (XAI,  "xai( ?usd)?|altcoins?( ?index)?")
  , (BAT,  "bat( ?usd)?|basic attention token")
  , (BCH,  "bch( ?usd)?|bitcoin( ?cash)")
  , (DASH, "dash( ?usd)?|darkcoin")
  , (EOS,  "eos(\\.io| ?io| ?usd)?")
  , (ETH,  "ether(ium|eum)?|eth( ?usd)?")
  , (ETC,  "ether(ium|eum) classic|etc( ?usd)?")
  , (IOTA, "iota?( ?usd)?")
  , (LTC,  "litecoin|ltc( ?usd)?|Ł")
  , (XMR,  "monero|xmr( ?usd)?")
  , (NEO,  "neo( ?usd)?")
  , (OMG,  "omisego|omg( ?usd)?")
  , (XRP,  "ripp?le?|ripple|xrp( ?usd)?")
  , (XLM,  "stell?ar( lumen)?|xlm( ?usd)?")
  , (ZEC,  "zcash|zec( ?usd)?")
  ]


data CryptocurrencyDimension = CryptocurrencyDimension deriving (Eq, Show, Typeable)

instance CustomDimension CryptocurrencyDimension where
  type DimensionData CryptocurrencyDimension = AmountOfCryptocurrencyData
  dimRules _ = [cryptocurrencyWithAmountPrefixed] ++ cryptocurrencyNameRules
  dimLangRules _ _ = []
  dimLocaleRules _ _ = []
  dimDependents _ = HashSet.empty


data AmountOfCryptocurrencyData = AmountOfCryptocurrencyData
  { amount         :: Maybe Double
  , cryptocurrency :: Cryptocurrency
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)


instance Resolve AmountOfCryptocurrencyData where
  type ResolvedValue AmountOfCryptocurrencyData = AmountOfCryptocurrencyValue
  resolve _ _ AmountOfCryptocurrencyData{..} = Just
    ( AmountOfCryptocurrencyValue amount cryptocurrency
    , False )


data AmountOfCryptocurrencyValue = AmountOfCryptocurrencyValue
  { vAmount         :: Maybe Double
  , vCryptocurrency :: Cryptocurrency
  }
  deriving (Eq, Ord, Show)

instance ToJSON AmountOfCryptocurrencyValue where
  toJSON (AmountOfCryptocurrencyValue{..}) = object
    [ "amount" .= vAmount
    , "unit"   .= vCryptocurrency
    ]


cryptocurrencyNameRules :: [Rule]
cryptocurrencyNameRules =
    map (\(crypto, re) ->
        Rule
          { name = "Cryptocurrency " <> TS.showt crypto
          , pattern = [regex re]
          , prod = \case
              (_:_) -> Just . Token (CustomDimension CryptocurrencyDimension) $ AmountOfCryptocurrencyData
                         { amount = Nothing
                         , cryptocurrency = crypto
                         }
              _ -> Nothing
          }
        )
        cryptocurrencies


isCryptocurrencyOnly :: Predicate
isCryptocurrencyOnly (Token (CustomDimension (dim :: a)) dimData)
  | Just Refl <- eqT @a @CryptocurrencyDimension, AmountOfCryptocurrencyData{..} <- dimData =
      amount == Nothing
isCryptocurrencyOnly _ = False


cryptocurrencyWithAmountPrefixed :: Rule
cryptocurrencyWithAmountPrefixed = Rule
  { name = "Cryptocurrency with amount (prefixed)"
  , pattern =
    [ Predicate isPositive
    , Predicate isCryptocurrencyOnly
    ]
  , prod = \case
      ((Token Numeral NumeralData{TNumeral.value = v}):
       (Token (CustomDimension (dim :: a)) dimData):
       _)
        | Just Refl <- eqT @a @CryptocurrencyDimension, AmountOfCryptocurrencyData{..} <- dimData ->
            Just . Token (CustomDimension CryptocurrencyDimension) $ AmountOfCryptocurrencyData
              { amount = Just v
              , cryptocurrency = cryptocurrency
              }
      _ -> Nothing
  }


main :: IO ()
main = do
  let en = makeLocale EN Nothing
  debug en "testing 10 bitcoin my dimension" [Seal Numeral, Seal (CustomDimension CryptocurrencyDimension)] >>= pPrint
  -- debug en "testing bitcoin my dimension pattern match" [Seal (CustomDimension CryptocurrencyDimension)] >>= print
