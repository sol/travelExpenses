module Monadic (Expenses, run, run_, payed, for, and, All(..), but) where

import Prelude hiding (and)

import           Control.Monad.Trans.Writer

import           TravelExpenses hiding (run, run_, for)
import qualified TravelExpenses

import           Party

newtype ExpensesM person a = ExpensesM { runExpensesM :: Writer [PayedFor person] a }

instance Monad (ExpensesM person) where
  m >>= f = ExpensesM (runExpensesM m >>= runExpensesM . f)
  return = ExpensesM . return

type Expenses person = ExpensesM person ()

infixl 7 `for`
for :: (IsParty a person) => Payed person -> a -> Expenses person
for p party = ExpensesM . tell . return $ p `TravelExpenses.for` toList party

run :: (Show person, Eq person) => Expenses person -> IO ()
run = TravelExpenses.run . snd . runWriter . runExpensesM

run_ :: (Eq person) => Expenses person -> [Owes person]
run_ = TravelExpenses.run_ . snd . runWriter . runExpensesM
