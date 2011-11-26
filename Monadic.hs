module Monadic (Expenses, run, run_, payed, for) where

import           Control.Monad.Trans.Writer

import           TravelExpenses hiding (run, run_)
import qualified TravelExpenses

newtype ExpensesM person a = ExpensesM { runExpensesM :: Writer [PayedFor person] a }

instance Monad (ExpensesM person) where
  m >>= f = ExpensesM (runExpensesM m >>= runExpensesM . f)
  return = ExpensesM . return

type Expenses person = ExpensesM person ()

payed :: person -> Rational -> [person] -> Expenses person
payed p a = ExpensesM . tell . return . Payed p a

run :: (Show person, Eq person) => Expenses person -> IO ()
run = TravelExpenses.run . snd . runWriter . runExpensesM

run_ :: (Eq person) => Expenses person -> [Owes person]
run_ = TravelExpenses.run_ . snd . runWriter . runExpensesM
