module Main where

import Midnight.GlacierDrop.Contrib.MpfCliSpec qualified as MpfCliSpec
import Test.Hspec

main :: IO ()
main = do
  mpfCliSpec <- MpfCliSpec.mkSpec
  hspec $ do
    describe "Midnight.GlacierDrop.Contrib Tests" $ do
      mpfCliSpec
