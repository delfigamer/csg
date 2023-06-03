{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MonoidMap where

import Data.Group.Class
import qualified Data.Map as Map

newtype MonoidMap k v = MonoidMap
    { unwrapMonoidMap :: Map.Map k v
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
    MonoidMap m1 <> MonoidMap m2 =
        MonoidMap $ Map.unionWith (<>) m1 m2

instance (Ord k, Semigroup v) => Monoid (MonoidMap k v) where
    mempty = MonoidMap Map.empty

instance (Ord k, Group v) => Group (MonoidMap k v) where
    inverse (MonoidMap m) = MonoidMap $ fmap inverse m

empty :: MonoidMap k v
empty = MonoidMap Map.empty

singleton :: k -> v -> MonoidMap k v
singleton k v = MonoidMap $ Map.singleton k v
