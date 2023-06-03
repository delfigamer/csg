module Data.NubList where

newtype NubList a = NubList [a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Eq a) => Semigroup (NubList a) where
    NubList xs <> NubList ys =
        NubList (filter (\x -> notElem x ys) xs <> ys)

instance (Eq a) => Monoid (NubList a) where
    mempty = NubList []
