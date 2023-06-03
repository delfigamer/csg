module Data.IntervalMapSpec where

import Data.Interval
import Instances
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.IntervalMap.ITree as ITree
import qualified Data.IntervalMap.RangeMap as RangeMap
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map.Merge

newtype MultiSet v = MultiSet (Map.Map v Int)
    deriving (Show, Eq, Ord)

instance (Ord v) => Semigroup (MultiSet v) where
    MultiSet vm1 <> MultiSet vm2 =
        MultiSet $
            Map.Merge.merge
                Map.Merge.preserveMissing
                Map.Merge.preserveMissing
                (Map.Merge.zipWithMaybeMatched $ \_ n1 n2 -> do
                    let nr = n1 + n2
                    if nr == 0
                        then Nothing
                        else Just nr
                )
                vm1
                vm2

instance (Ord v) => Monoid (MultiSet v) where
    mempty = MultiSet Map.empty

instance (Ord v) => RangeMap.Group (MultiSet v) where
    inverse (MultiSet ms) = MultiSet $ fmap negate ms

singletonMultiset :: v -> MultiSet v
singletonMultiset x = MultiSet $ Map.singleton x 1



newtype Sum a = Sum a
    deriving (Show, Eq, Ord)

instance (Num a) => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance (Num a) => Monoid (Sum a) where
    mempty = Sum 0

instance (Num a) => RangeMap.Group (Sum a) where
    inverse (Sum x) = Sum (negate x)



spec :: Spec
spec = do
    describe "Data.IntervalMap" $ do
        describe "ITree and RangeMap match" $ do
            prop "on MultiSets" $ \(LongerList intervals) -> do
                let _ = intervals :: [(Interval Int, Int)]
                let rangeMap =
                        foldMap
                            (\(ab, c) ->
                                RangeMap.singleton ab (singletonMultiset c)
                            )
                            intervals
                let rangeMapResult =
                        RangeMap.toIntervalList rangeMap
                let itree =
                        foldMap
                            (\(ab, c) ->
                                ITree.singleton ab (singletonMultiset c)
                            )
                            intervals
                let itreeResult =
                        ITree.toIntervalList itree
                shouldBe itreeResult rangeMapResult
            prop "on Sums" $ \(LongerList intervals) -> do
                let _ = intervals :: [(Interval Int, Int)]
                let rangeMap =
                        foldMap
                            (\(ab, c) -> RangeMap.singleton ab (Sum c))
                            intervals
                let rangeMapResult =
                        RangeMap.toIntervalList rangeMap
                let itree =
                        foldMap
                            (\(ab, c) -> ITree.singleton ab (Sum c))
                            intervals
                let itreeResult =
                        ITree.toIntervalList itree
                shouldBe itreeResult rangeMapResult
