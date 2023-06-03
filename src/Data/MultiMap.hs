module Data.MultiMap where

import Data.Group.Class
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map.Merge

data WithMultiplicity a
    = WithMultiplicity
        { multNumber :: {-# UNPACK #-} !Int
        , multValue :: a
        }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype MultiMap k v = MultiMap
    { multiplicityMap :: Map.Map k (WithMultiplicity v)
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord k) => Semigroup (MultiMap k v) where
    MultiMap mm1 <> MultiMap mm2 =
        MultiMap $
            Map.Merge.merge
                Map.Merge.preserveMissing
                Map.Merge.preserveMissing
                (Map.Merge.zipWithMaybeMatched $ \_ x1 x2 -> do
                    let nr = multNumber x1 + multNumber x2
                    if nr == 0
                        then Nothing
                        else Just $
                            WithMultiplicity
                                nr
                                (multValue x1)
                )
                mm1
                mm2

instance (Ord k) => Monoid (MultiMap k v) where
    mempty = MultiMap Map.empty

instance (Ord k) => Group (MultiMap k v) where
    inverse (MultiMap mm) =
        MultiMap $
            fmap
                (\x -> x {multNumber = negate (multNumber x)})
                mm

empty :: MultiMap k v
empty = MultiMap Map.empty

singleton :: k -> v -> MultiMap k v
singleton k v = MultiMap $ Map.singleton k (WithMultiplicity 1 v)

fromMap :: Map.Map k v -> MultiMap k v
fromMap m = MultiMap $ fmap (WithMultiplicity 1) m

toMap :: MultiMap k v -> Map.Map k v
toMap (MultiMap mm) = fmap multValue mm

toList :: MultiMap k v -> [(k, v)]
toList (MultiMap mm) =
    fmap (\(k, x) -> (k, multValue x)) $
    Map.toList mm

elems :: MultiMap k v -> [v]
elems (MultiMap mm) =
    fmap multValue $
    Map.elems mm
