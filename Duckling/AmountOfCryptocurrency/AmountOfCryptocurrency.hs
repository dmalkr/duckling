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

module Duckling.AmountOfCryptocurrency.AmountOfCryptocurrency where


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

import Duckling.Numeral.Helpers (isPositive)
import Duckling.Numeral.Types (NumeralData (..))
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


data AmountOfCryptocurrencyDimension = AmountOfCryptocurrencyDimension deriving (Eq, Show, Typeable)

instance CustomDimension AmountOfCryptocurrencyDimension where
  type DimensionData AmountOfCryptocurrencyDimension = AmountOfCryptocurrencyData
  dimRules _ = [cryptocurrencyWithAmountPrefixed, cryptocurrencyWithAmountSuffixed] ++ cryptocurrencyNameRules
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
              (_:_) -> Just . Token (CustomDimension AmountOfCryptocurrencyDimension) $ AmountOfCryptocurrencyData
                         { amount = Nothing
                         , cryptocurrency = crypto
                         }
              _ -> Nothing
          }
        )
        cryptocurrencies


isCryptocurrencyOnly :: Predicate
isCryptocurrencyOnly (Token (CustomDimension (dim :: a)) dimData)
  | Just Refl <- eqT @a @AmountOfCryptocurrencyDimension, AmountOfCryptocurrencyData{..} <- dimData =
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
        | Just Refl <- eqT @a @AmountOfCryptocurrencyDimension, AmountOfCryptocurrencyData{..} <- dimData ->
            Just . Token (CustomDimension AmountOfCryptocurrencyDimension) $ AmountOfCryptocurrencyData
              { amount = Just v
              , cryptocurrency = cryptocurrency
              }
      _ -> Nothing
  }


cryptocurrencyWithAmountSuffixed :: Rule
cryptocurrencyWithAmountSuffixed = Rule
  { name = "Cryptocurrency with amount (suffixed)"
  , pattern =
    [ Predicate isCryptocurrencyOnly
    , Predicate isPositive
    ]
  , prod = \case
      ((Token (CustomDimension (dim :: a)) dimData):
       (Token Numeral NumeralData{TNumeral.value = v}):
       _)
        | Just Refl <- eqT @a @AmountOfCryptocurrencyDimension, AmountOfCryptocurrencyData{..} <- dimData ->
            Just . Token (CustomDimension AmountOfCryptocurrencyDimension) $ AmountOfCryptocurrencyData
              { amount = Just v
              , cryptocurrency = cryptocurrency
              }
      _ -> Nothing
  }

amountOfCryptocurrencyFullDimension :: [Seal Dimension]
amountOfCryptocurrencyFullDimension = [Seal Numeral, Seal (CustomDimension AmountOfCryptocurrencyDimension)]
