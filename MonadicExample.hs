module Main (main) where

import Prelude hiding (and)
import Monadic

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq, Enum, Bounded)

main = run $ do
    Hans  `payed` 500 `for` Klaus
    Klaus `payed` 510 `for` Hans
    Hans  `payed`  10 `for` Erna `and` Elke
