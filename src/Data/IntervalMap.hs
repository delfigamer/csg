module Data.IntervalMap
    ( IntervalMap
    , toIntervalList
    , empty
    , singleton
    , foldMapIntervals
    )
where

import Data.Interval
import Data.IntervalMap.EndpointMap

foldMapIntervals ::
    (Monoid v, Monoid m) =>
    (Interval k -> v -> m) ->
    IntervalMap k v ->
    m
foldMapIntervals fn em =
    foldMap (uncurry fn) (toIntervalList em)
