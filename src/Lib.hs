module Lib
    ( someFunc
    )
where

import           GHC.TypeLits
import           Data.Proxy

someFunc :: forall  (n :: Nat) . (KnownNat n) => Proxy n -> Integer
someFunc _ = natVal (Proxy @n) + natVal (Proxy @(n + 2))
