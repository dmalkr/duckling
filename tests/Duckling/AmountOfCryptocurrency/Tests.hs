module Duckling.AmountOfCryptocurrency.Tests
  ( tests
  ) where


import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit


import Duckling.Testing.Asserts
import Duckling.AmountOfCryptocurrency.AmountOfCryptocurrency
import Duckling.AmountOfCryptocurrency.Corpus


tests :: TestTree
tests = testGroup "AmountOfCryptocurrency Tests"
  [ makeCorpusTest amountOfCryptocurrencyFullDimension corpus
  ]
