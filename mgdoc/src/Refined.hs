{-# LANGUAGE TemplateHaskell #-}

module Refined (Ascending, makeAscending, unAscending, unsafeMakeAscending) where

import Data.Coerce (coerce)
import PlutusTx (makeLift)
import PlutusTx.List (sort)
import PlutusTx.Prelude (Ord, (.))
import Text.Show qualified

newtype Ascending a = Ascending a deriving (Text.Show.Show)
makeLift ''Ascending

-- | This doesn't check if the list is sorted, so you must be careful to only use this with lists that are guaranteed to be sorted
-- ie. you construct a list such that when constructing it you guarantee that it is sorted.
{-# INLINEABLE unsafeMakeAscending #-}
unsafeMakeAscending :: [a] -> Ascending [a]
unsafeMakeAscending = Ascending

makeAscending :: (Ord a) => [a] -> Ascending [a]
makeAscending = Ascending . sort

unAscending :: Ascending a -> a
unAscending = coerce
