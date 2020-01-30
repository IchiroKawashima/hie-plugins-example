module Lib
    ( knownNatExample
    , natNormalizeExample
    , extraExample
    )
where

import           GHC.TypeLits
import           GHC.TypeLits.Extra
import           Data.Proxy

knownNatExample :: forall (n :: Nat) . (KnownNat n) => Integer
knownNatExample = natVal (Proxy @n) + natVal (Proxy @(n + 2))

natNormalizeExample :: forall (x :: Nat) (y :: Nat)
                     . (KnownNat x, KnownNat y)
                    => Proxy ((x + 2) ^ (y + 2))
natNormalizeExample = Proxy :: Proxy (4 * x * (2 + x) ^ y + 4 * (2 + x) ^ y + (2 + x) ^ y * x ^ 2)

extraExample :: forall (x :: Nat) (y :: Nat) . (KnownNat x, KnownNat y) => Proxy (x ^ Log x y)
extraExample = Proxy :: Proxy y
