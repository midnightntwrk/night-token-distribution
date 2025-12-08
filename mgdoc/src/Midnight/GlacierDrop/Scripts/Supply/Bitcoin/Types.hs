{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Midnight.GlacierDrop.Scripts.Supply.Bitcoin.Types where

import GHC.Generics (Generic)
import PlutusTx.Prelude
import Prelude qualified as Haskell

newtype MessageHash = MessageHash BuiltinByteString
  deriving stock (Generic)
  deriving newtype (ToData, FromData, Haskell.Show, Haskell.Eq)
