{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
module Spec (main) where

import           Test.Hspec.Monadic
import           Test.Hspec.HUnit ()
import           Test.HUnit

import           Control.Monad
import           Data.List

import           Monadic
import           TravelExpenses (Owes(..))

shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = unless (actual == expected) (assertFailure message)
  where
    message = show actual ++ " was not equal to " ++ show expected

shouldBeSet :: (Show a, Ord a) => [a] -> [a] -> Assertion
actual `shouldBeSet` expected = sort actual `shouldBe` sort expected

deriving instance (Ord person) => Ord (Owes person)

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq, Ord)

to = ($)

main = hspec $ do

  describe "expenses" $ do

    it "paying for yourself does not result in any debts" $ do
      run_ $ do
        Hans  `payed` 500 `for` [Hans]
      `shouldBeSet` []

    it "your buddy owes you 2/3 of the total amount, if you pay once for yourself and twice for him" $ do
      run_ $ do
        Hans  `payed` 300 `for` [Hans, Klaus, Klaus]
      `shouldBeSet` [Klaus `Owes` 200 `to` Hans]
