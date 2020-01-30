module Lib
    ( knownNatExample
    )
where

import           GHC.TypeLits
import           Data.Proxy

knownNatExample :: forall (n :: Nat) . (KnownNat n) => Proxy n -> Integer
knownNatExample _ = natVal (Proxy @n) + natVal (Proxy @(n + 2))

