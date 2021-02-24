module Main (main) where

import Text.Pretty.Simple

import Duckling.Debug
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.AmountOfCryptocurrency.AmountOfCryptocurrency


main :: IO ()
main = do
  let en = makeLocale EN Nothing
  debug en "10 bitcoin" amountOfCryptocurrencyFullDimension >>= pPrint
