module Midnight.GlacierDrop.Contrib.ShowCBORHex where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR, natVersion, serialize')
import Data.ByteString.Base16 qualified as Base16

newtype ShowCBORHex a = ShowCBORHex a

instance (DecCBOR a, EncCBOR a) => Show (ShowCBORHex a) where
  showsPrec p (ShowCBORHex a) =
    showParen (p > 0) $
      showString
        "ShowCBORHex $ Cardano.Ledger.Binary.Decoding.unsafeDeserialize' (Cardano.Ledger.Binary.Decoding.natVersion @11) $ Data.ByteString.Base16.decodeLenient $ "
        . showsPrec 1 (Base16.encode $ serialize' (natVersion @11) $ a)
