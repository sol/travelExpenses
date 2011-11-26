{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
module Spec (main) where

import           Prelude hiding (and)

import           Test.Hspec.Monadic
import           Test.Hspec.HUnit ()
import           Test.HUnit

import           Control.Monad

import           Party

infixl 0 `shouldBe`
shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = unless (actual == expected) (assertFailure message)
  where
    message = show actual ++ " was not equal to " ++ show expected

deriving instance Eq a => Eq (Party a)
infixl 0 `shouldBeParty`
shouldBeParty :: (IsParty a b, Show b, Ord b) => a -> [b] -> Assertion
shouldBeParty p l = toList p `shouldBe` l

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq, Ord, Enum, Bounded)

main = hspec $ do

  describe "list" $ do
    it "is a party" $ do
      [Hans, Klaus] `shouldBeParty` [Hans, Klaus]

  describe "person" $ do
    it "is a party" $ do
      Elke `shouldBeParty` [Elke]

  describe "all" $ do
    it "is a party, which contains each person once" $ do
      All `shouldBeParty` [Hans, Klaus, Erna, Elke]

  describe "and" $ do
    it "combines two persons into a party" $ do
      Hans `and` Klaus `shouldBeParty` [Hans, Klaus]

  describe "but" $ do

    it "removes a person from a party" $ do
      All `but` Klaus `shouldBeParty` [Hans, Erna, Elke]
