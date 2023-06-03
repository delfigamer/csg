module Data.IntervalMap.EndpointMap where

import Data.Group.Class
import Data.Interval
import qualified Data.Map as Map

newtype IntervalMap k v = EndpointMap (Map.Map k v)
    deriving (Show, Eq, Ord)

instance (Ord k, Eq v, Group v) => Semigroup (IntervalMap k v) where
    EndpointMap rm1 <> EndpointMap rm2 =
        EndpointMap $
            Map.unionWith (<>) rm1 rm2

instance (Ord k, Eq v, Group v) => Monoid (IntervalMap k v) where
    mempty = EndpointMap Map.empty

toIntervalList ::
    (Monoid v) =>
    IntervalMap k v ->
    [(Interval k, v)]
toIntervalList (EndpointMap rm) =
    go mempty (Map.toList rm)
  where
    go ms ((k1, delta) : rest@((k2, _) : _)) = do
        let ms2 = ms <> delta
        (Interval k1 k2, ms2) : go ms2 rest
    go _ _ =
        []

empty ::
    IntervalMap k v
empty =
    EndpointMap Map.empty

singleton ::
    (Ord k, Group v) =>
    Interval k ->
    v ->
    IntervalMap k v
singleton (Interval k1 k2) v
    | k1 < k2 =
        EndpointMap $
            Map.singleton k1 v <>
            Map.singleton k2 (inverse v)
    | otherwise =
        EndpointMap Map.empty
