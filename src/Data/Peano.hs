{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Peano
    ( P (..)
    , PFromNat
    , PI (ZI, SI)
    , KnownP (..)
    , withKnownP
    , Add
    , addPI
    , ComparePI (..)
    , comparePI
    , mkPI
    , piToInt
    , withPIFromInt
    )
where

import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce

data P = Z | S P

type family PFromNat (n :: Nat) where
    PFromNat 0 = 'Z
    PFromNat p = 'S (PFromNat (p - 1))

newtype PI (p :: P) = PI# Int

instance Show (PI p) where
    showsPrec d (PI# x) =
        showParen (d > 10) $
            showString "mkPI @" .
            showsPrec 11 x

pattern ZI ::
    forall p. () =>
    (p ~ 'Z) =>
    PI p
pattern ZI <- (matchPI -> MatchZI)
  where
    ZI = PI# 0

pattern SI ::
    forall p. () =>
    forall q. (p ~ ('S q)) =>
    PI q ->
    PI p
pattern SI x <- (matchPI -> MatchSI x)
  where
    SI (PI# d) = PI# (d + 1)

{-# COMPLETE ZI, SI #-}

data MatchPI p where
    MatchZI :: MatchPI 'Z
    MatchSI :: PI p -> MatchPI ('S p)

matchPI :: PI p -> MatchPI p
matchPI (PI# d)
    | d <= 0 = unsafeCoerce MatchZI
    | otherwise = unsafeCoerce $ MatchSI $ PI# (d - 1)

class KnownP p where
    knownPI :: PI p

instance KnownP 'Z where
    knownPI = ZI

instance (KnownP p) => KnownP ('S p) where
    knownPI = SI knownPI

withKnownP :: forall p a. (KnownP p => a) -> (PI p -> a)
withKnownP = unsafeCoerce @(KnownP p => a) @(PI p -> a)

type family Add pa pb where
    Add 'Z pb = pb
    Add ('S pa) pb = 'S (Add pa pb)

addPI :: PI pa -> PI pb -> PI (Add pa pb)
addPI (PI# a) (PI# b) = PI# (a + b)

data ComparePI pa pb where
    LTPI :: PI pd -> ComparePI pa ('S (Add pd pa))
    EQPI :: ComparePI p p
    GTPI :: PI pd -> ComparePI ('S (Add pd pb)) pb

deriving instance Show (ComparePI pa pb)

comparePI :: PI pa -> PI pb -> ComparePI pa pb
comparePI (PI# x) (PI# y)
    | x < y =
        unsafeCoerce $ LTPI (PI# (y - x - 1))
    | x > y =
        unsafeCoerce $ GTPI (PI# (x - y - 1))
    | otherwise =
        unsafeCoerce EQPI

mkPI :: forall n. (KnownNat n) => PI (PFromNat n)
mkPI = PI# (fromInteger $ natVal $ Proxy @n)

piToInt :: PI p -> Int
piToInt (PI# d) = d

withPIFromInt :: Int -> (forall p. PI p -> a) -> a
withPIFromInt d fn = fn (PI# d)
