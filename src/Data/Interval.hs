{-# LANGUAGE DerivingVia #-}

module Data.Interval where

import Data.Coerce
import Data.Functor.Identity
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

data Extended a
    = NegInfinity
    | Interior a
    | PosInfinity
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

fromExtended :: a -> a -> Extended a -> a
fromExtended onNegInf onPosInf ex =
    case ex of
        NegInfinity -> onNegInf
        Interior x -> x
        PosInfinity -> onPosInf

instance Bounded (Extended a) where
    minBound = NegInfinity
    maxBound = PosInfinity

data Interval a = Interval a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

Aeson.deriveJSON Aeson.defaultOptions ''Interval

class IsInterval a where
    intersect :: a -> a -> a
    hull :: a -> a -> a
    isEmptySet :: a -> Bool
    overlaps :: a -> a -> Bool
    overlaps x y = not (isEmptySet (intersect x y))

class (IsInterval a) => HasEmptySet a where
    emptySet :: a

class (IsInterval a) => HasFullSet a where
    fullSet :: a

instance (Ord a) => IsInterval (Interval a) where
    intersect (Interval loa hia) (Interval lob hib) =
        Interval (max loa lob) (min hia hib)
    hull (Interval loa hia) (Interval lob hib) =
        Interval (min loa lob) (max hia hib)
    isEmptySet (Interval a b) = a > b

instance (Ord a, Bounded a) => HasEmptySet (Interval a) where
    emptySet = Interval maxBound minBound

instance (Ord a, Bounded a) => HasFullSet (Interval a) where
    fullSet = Interval minBound maxBound

intervalFrom :: (Bounded a) => a -> Interval a
intervalFrom x = Interval x maxBound

intervalTo :: (Bounded a) => a -> Interval a
intervalTo x = Interval minBound x

intervalContains :: (Ord a) => a -> Interval a -> Bool
intervalContains x (Interval a b) = a <= x && x <= b

newtype Intersection a = Intersection { getIntersection :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving (Applicative, Monad) via Identity

instance (IsInterval a) => Semigroup (Intersection a) where
    (<>) = coerce (intersect :: a -> a -> a)

instance (HasFullSet a) => Monoid (Intersection a) where
    mempty = coerce (fullSet :: a)

newtype Hull a = Hull { getHull :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving (Applicative, Monad) via Identity

instance (IsInterval a) => Semigroup (Hull a) where
    (<>) = coerce (hull :: a -> a -> a)

instance (HasEmptySet a) => Monoid (Hull a) where
    mempty = coerce (emptySet :: a)
