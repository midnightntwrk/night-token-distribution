module Midnight.GlacierDrop.Contrib.Data.List where

-- FIXME: Drop this helper when we bump GHC (Data.List should provide it)
(!?) :: [a] -> Int -> Maybe a
{-# INLINEABLE (!?) #-}
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n
