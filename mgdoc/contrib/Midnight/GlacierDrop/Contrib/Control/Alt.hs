module Midnight.GlacierDrop.Contrib.Control.Alt where

-- | A PureScript inspired `Alt` which `Alternative` could be built on top of.
-- | At the end `Alternative` seems to have limited use cases and `Alt` is more flexible.
-- | https://pursuit.purescript.org/packages/purescript-control/3.0.0/docs/Control.Alt
class Alt f where
  -- | Associativity: (x <|> y) <|> z == x <|> (y <|> z)
  -- | Distributivity: f <$> (x <|> y) == (f <$> x) <|> (f <$> y)
  alt :: f a -> f a -> f a

instance Alt (Either e) where
  alt (Left _) r = r
  alt l _ = l

instance Alt Maybe where
  alt (Just l) _ = Just l
  alt Nothing r = r

instance Alt [] where
  alt = (++)

(<||>) :: (Alt f) => f a -> f a -> f a
(<||>) = alt

infixl 3 <||>
