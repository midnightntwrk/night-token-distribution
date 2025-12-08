module Midnight.GlacierDrop.Contrib.IO.Exception where

import System.Exit (die)

fromRightThrowing :: (Show a) => Either a b -> IO b
fromRightThrowing = either (die . show) pure
