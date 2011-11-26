-- * A group of people makes a trip.
-- * Repeatedly: someone pays a bill for a subset of the group.
-- * How much does anyone owe to anyone else?

module TravelExpenses where

import Text.Printf

for :: Payed a -> [a] -> PayedFor a
for = For

payed :: a -> Rational -> Payed a
payed = Payed

-- Input: payer, amount, receivers
data Payed a = a `Payed` Rational
    deriving Show

data PayedFor a = (Payed a) `For` [a]
    deriving Show

-- Output: the first person owes an amount to the second person
data Owes a = Owes a Rational a
    deriving Eq

instance Show a => Show (Owes a) where
    show (Owes pA amount pB) =
           (show pA) ++ " owes "
        ++ (printf "%.2f" ((fromRational amount)::Double))
        ++ " to " ++ (show pB)

-- turns a payment into a list of debts
pays2owes :: PayedFor a -> [Owes a]
pays2owes (For (Payed payer amount) receivers) =
    [Owes receiver fraction payer | receiver <- receivers]
        where fraction = amount / (fromIntegral $ length receivers)

allDebts = concatMap pays2owes

-- processOne (x, nonMatchingDebts) d
-- If the people of x and d match, absorb d into x
-- else add it to the list of nonMatchingDebts
processOne :: Eq a => (Owes a, [Owes a]) -> Owes a -> (Owes a, [Owes a])
processOne (x@(Owes personA amount personB), nonMatchingDebts) d@(Owes pA am pB)
    | personA == pA && personB == pB = ((Owes personA (amount + am) personB), nonMatchingDebts)
    | personA == pB && personB == pA = ((Owes personA (amount - am) personB), nonMatchingDebts)
    | otherwise                      = (x, d:nonMatchingDebts)

processAll :: Eq a => [Owes a] -> [Owes a]
processAll [] = []
processAll (d:ds) = let (e, es) = (foldl processOne (d, []) ds) in
    e : (processAll es)

-- if the amount of a debt is negative, flip the two persons and invert the amount
flipDebt :: Owes a -> Owes a
flipDebt (Owes pA amount pB) =
    if( amount < 0 )
        then (Owes pB (-amount) pA)
        else (Owes pA amount pB)

run_ :: Eq a => [PayedFor a] -> [Owes a]
run_ = map flipDebt . filter (\(Owes a _ b) -> a /= b) . processAll . allDebts

run :: (Show a, Eq a) => [PayedFor a] -> IO ()
run = mapM_ print . run_
