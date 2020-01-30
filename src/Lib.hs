module Lib
    ( knownNatExample
    , natNormalizeExample
    , extraExample
    )
where

import           GHC.TypeLits
import           GHC.TypeLits.Extra
import           Data.Proxy

-- • Could not deduce (KnownNat (n + 2))
--     arising from a use of ‘natVal’
--   from the context: KnownNat n
--     bound by the type signature for:
--                knownNatExample :: forall (n :: Nat). KnownNat n => Integer
knownNatExample :: forall (n :: Nat) . (KnownNat n) => Integer
knownNatExample = natVal (Proxy @n) + natVal (Proxy @(n + 2))

-- • Couldn't match type ‘(((4 * x) * ((2 + x) ^ y))
--                         + (4 * ((2 + x) ^ y)))
--                        + (((2 + x) ^ y) * (x ^ 2))’
--                  with ‘(x + 2) ^ (y + 2)’
--   Expected type: Proxy
--                    ((((4 * x) * ((2 + x) ^ y)) + (4 * ((2 + x) ^ y)))
--                     + (((2 + x) ^ y) * (x ^ 2)))
--     Actual type: Proxy ((x + 2) ^ (y + 2))
natNormalizeExample :: forall (x :: Nat) (y :: Nat)
                     . (KnownNat x, KnownNat y)
                    => Proxy ((x + 2) ^ (y + 2))
natNormalizeExample = Proxy :: Proxy (4 * x * (2 + x) ^ y + 4 * (2 + x) ^ y + (2 + x) ^ y * x ^ 2)

-- • Couldn't match type ‘y’ with ‘x ^ Log x y’
--   ‘y’ is a rigid type variable bound by
--     the type signature for:
--       extraExample :: forall (x :: Nat) (y :: Nat).
--                       (KnownNat x, KnownNat y) =>
--                       Proxy (x ^ Log x y)
extraExample :: forall (x :: Nat) (y :: Nat) . (KnownNat x, KnownNat y) => Proxy (x ^ Log x y)
extraExample = Proxy :: Proxy y
