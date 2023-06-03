module Data.MinMaxWith where

data MinWith a b = MinWith a b
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord a, Semigroup b) => Semigroup (MinWith a b) where
    m1@(MinWith a1 b1) <> m2@(MinWith a2 b2) =
        case compare a1 a2 of
            LT -> m1
            EQ -> MinWith a1 (b1 <> b2)
            GT -> m2

instance (Bounded a, Ord a, Monoid b) => Monoid (MinWith a b) where
    mempty = MinWith maxBound mempty

data MaxWith a b = MaxWith a b
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord a, Semigroup b) => Semigroup (MaxWith a b) where
    m1@(MaxWith a1 b1) <> m2@(MaxWith a2 b2) =
        case compare a1 a2 of
            LT -> m2
            EQ -> MaxWith a1 (b1 <> b2)
            GT -> m1

instance (Bounded a, Ord a, Monoid b) => Monoid (MaxWith a b) where
    mempty = MaxWith minBound mempty
