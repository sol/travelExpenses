module Main (main) where

import TravelExpenses

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq)

main = run
    [ Hans  `Payed` 500 `for` [Klaus]
    , Klaus `Payed` 510 `for` [Hans]
    , Hans  `Payed` 10 `for` [Erna, Elke]
    ]
