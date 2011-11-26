{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Party where

import qualified Data.List as List

class IsParty a b where
  toList :: a -> [b]

data Party a = Party [a]
  deriving Show
instance IsParty (Party a) a where
  toList (Party l) = l

data All = All
instance (Bounded a, Enum a) => IsParty All a where
  toList _ = [minBound .. maxBound]

instance IsParty a a where
  toList = return

instance IsParty [a] a where
  toList = id

and :: (IsParty a b) => a -> b -> Party b
and a b = Party $ toList a ++ [b]

infixl 8 `but`
but :: (Eq b, IsParty a b) => a -> b -> Party b
but a b = Party $ b `List.delete` toList a
