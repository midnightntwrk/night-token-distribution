module Midnight.GlacierDrop.Contrib.Data.Foldable where

import Data.Foldable (foldlM)

foldMapM
  :: (Monad m, Monoid w, Foldable t)
  => (a -> m w)
  -> t a
  -> m w
foldMapM f =
  foldlM
    ( \acc a -> do
        w <- f a
        pure $! acc <> w
    )
    mempty

foldMapMFlipped
  :: (Monoid b) => (Monad m) => (Foldable f) => f a -> (a -> m b) -> m b
foldMapMFlipped = flip foldMapM
