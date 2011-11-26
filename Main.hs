module Main (main) where

import Monadic

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq)

main = run $ do
    Hans  `payed` 500 `for` [Klaus]
    Klaus `payed` 510 `for` [Hans]
    Hans  `payed` 10  `for` [Erna, Elke]
