module Data.CSeq where

import Data.Semigroup
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

foldrPairsInner :: (a -> a -> b -> b) -> b -> Seq a -> b
foldrPairsInner _ seed Empty =
    seed
foldrPairsInner f seed (xs :|> xLast) =
    snd $ foldr (\x ~(y, r) -> (x, f x y r)) (xLast, seed) xs

foldMapPairsInner :: (Monoid m) => (a -> a -> m) -> Seq a -> m
foldMapPairsInner tom =
    foldrPairsInner (\a b r -> tom a b <> r) mempty

mapPairsInner :: (a -> a -> b) -> Seq a -> Seq b
mapPairsInner f =
    foldMapPairsInner (\a b -> Seq.singleton $ f a b)

foldrPairsLoop :: (a -> a -> b -> b) -> b -> Seq a -> b
foldrPairsLoop _ seed Empty =
    seed
foldrPairsLoop f seed xs@(xFirst :<| _) =
    snd $ foldr (\x ~(y, r) -> (x, f x y r)) (xFirst, seed) xs

foldMapPairsLoop :: (Monoid m) => (a -> a -> m) -> Seq a -> m
foldMapPairsLoop tom =
    foldrPairsLoop (\a b r -> tom a b <> r) mempty

mapPairsLoop :: (a -> a -> b) -> Seq a -> Seq b
mapPairsLoop f =
    foldMapPairsLoop (\a b -> Seq.singleton $ f a b)

forPairsLoopM_ :: (Applicative m) => Seq a -> (a -> a -> m ()) -> m ()
forPairsLoopM_ = flip traversePairsLoop_

traversePairsLoop_ :: (Applicative m) => (a -> a -> m ()) -> Seq a -> m ()
traversePairsLoop_ fn xs =
    foldrPairsLoop (\a b r -> fn a b *> r) (pure ()) xs

trySelectMinimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
trySelectMinimumOn f xs =
    fmap (\(Min (Arg _ x)) -> x) $
        foldMap
            (\x -> Just (Min (Arg (f x) x)))
            xs

tryTakeMinimumOn :: (Ord b) => (a -> b) -> Seq a -> Maybe (a, Seq a)
tryTakeMinimumOn f xs =
    fmap (\(Min (Arg _ result)) -> result) $
        foldMapExclusions
            (\prefix x suffix -> Just $ Min $ Arg (f x) (x, prefix <> suffix))
            xs

foldrExclusions ::
    (Seq a -> a -> Seq a -> b -> b) ->
    b ->
    Seq a ->
    b
foldrExclusions f seed = go Seq.empty
  where
    go _ Empty = seed
    go prefix (x :<| suffix) =
        f prefix x suffix $ go (prefix :|> x) suffix

foldMapExclusions ::
    (Monoid m) =>
    (Seq a -> a -> Seq a -> m) ->
    Seq a ->
    m
foldMapExclusions tom =
    foldrExclusions
        (\prefix x suffix acc -> tom prefix x suffix <> acc)
        mempty

foldrTriplesLoop :: (a -> a -> a -> b -> b) -> b -> Seq a -> b
foldrTriplesLoop _ seed Empty =
    seed
foldrTriplesLoop f seed (xs :|> xLast) =
    case xs of
        Empty ->
            f xLast xLast xLast seed
        xFirst :<| _ ->
            let (_, xSecond, rTemp) =
                    foldr
                        (\x ~(y, z, r) -> (x, y, f x y z r))
                        (xLast, xFirst, seed)
                        xs
            in
                f xLast xFirst xSecond rTemp

foldMapTriplesLoop :: (Monoid m) => (a -> a -> a -> m) -> Seq a -> m
foldMapTriplesLoop tom =
    foldrTriplesLoop (\a b c r -> tom a b c <> r) mempty

mapTriplesLoop :: (a -> a -> a -> b) -> Seq a -> Seq b
mapTriplesLoop f =
    foldMapTriplesLoop (\a b c -> Seq.singleton $ f a b c)

canonicalRotation ::
    (Ord a) =>
    Seq a ->
    Seq a
canonicalRotation Empty = Empty
canonicalRotation xs0 = minimum $ buildRotations Empty xs0
  where
    buildRotations _ Empty = []
    buildRotations ys xs@(x :<| rest) =
        (xs <> ys) : buildRotations (ys :|> x) rest
