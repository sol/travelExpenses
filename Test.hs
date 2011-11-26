{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Test where

import Test.QuickCheck
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Control.Applicative

import qualified Data.List as List

import TravelExpenses

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq, Enum, Bounded)

payedForAmount (For (Payed _ am) _) = am

instance Arbitrary Person where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary (Payed Person) where
    arbitrary = payed <$> arbitrary <*> arbitrary

instance Arbitrary (PayedFor Person) where
    arbitrary = for <$> arbitrary <*> receivers
      where
        receivers = elements $ tail $ List.subsequences [minBound .. maxBound]

sumDebts = sum . (map (\(Owes _ amount _) -> amount))
sumExpenses = sum . map payedForAmount

prop_sumOfOneExpense :: PayedFor Person -> Bool
prop_sumOfOneExpense expense = (payedForAmount expense) == (sumDebts $ pays2owes expense)

prop_sumOfLotsExpenses :: [PayedFor Person] -> Bool
prop_sumOfLotsExpenses expenses = (sumExpenses expenses) == (sumDebts $ allDebts expenses)

case_oneReceiver = ((Owes Klaus 200 Hans)    `elem` (allDebts [(Hans `payed` 200 `for` [Klaus])])) @=? True
case_twoReceivers = ((Owes Klaus 100 Hans)   `elem` (allDebts [(Hans `payed` 200 `for` [Klaus, Hans])])) @=? True
case_threeReceivers = ((Owes Klaus 100 Hans) `elem` (allDebts [(Hans `payed` 300 `for` [Klaus, Hans, Elke])])) @=? True
case_fourReceivers = ((Owes Klaus 50 Hans)   `elem` (allDebts [(Hans `payed` 200 `for` [Klaus, Hans, Elke, Erna])])) @=? True

main = $(defaultMainGenerator)
