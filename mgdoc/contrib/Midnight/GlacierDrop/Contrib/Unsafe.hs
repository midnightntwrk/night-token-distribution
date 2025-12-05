module Midnight.GlacierDrop.Contrib.Unsafe where

import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)

-- | Take a error message and a Maybe value and return the value if it is Just
-- | or throw an error with the message if it is Nothing.
unsafeFromJust :: String -> Maybe a -> a
unsafeFromJust msg = fromMaybe (error msg)

unsafeFromRightMsg :: String -> Either a b -> b
unsafeFromRightMsg msg = either (error . const msg) id

unsafeFromLeftMsg :: String -> Either a b -> a
unsafeFromLeftMsg msg = either id (error . const msg)

unsafeParseJSONMsg :: (A.FromJSON a) => String -> A.Value -> a
unsafeParseJSONMsg msg v = case A.fromJSON v of
  A.Success a -> a
  A.Error e -> error $ msg ++ ": " ++ e
