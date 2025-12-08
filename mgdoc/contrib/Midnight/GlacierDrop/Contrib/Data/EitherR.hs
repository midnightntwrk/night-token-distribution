{-# OPTIONS_GHC -Wno-orphans #-}

module Midnight.GlacierDrop.Contrib.Data.EitherR where

import Data.EitherR (EitherR (EitherR))

instance Foldable (EitherR a) where
  foldMap f (EitherR (Left err)) = f err
  foldMap _ _ = mempty

instance Traversable (EitherR a) where
  traverse f (EitherR (Left err)) = EitherR . Left <$> f err
  traverse _ (EitherR (Right x)) = pure $ EitherR (Right x)
