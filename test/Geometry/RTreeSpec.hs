{-# OPTIONS_GHC -Wno-orphans #-}

module Geometry.RTreeSpec where

import Control.Monad
import Data.Foldable
import Data.Interval
import Geometry.BBox
-- import Geometry.Vec3
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Set as Set
import qualified Geometry.RTree as RTree

processPairwiseDynamicList ::
    [a] ->
    [b] ->
    (a -> b -> ([a], [b])) ->
    ([a], [b])
processPairwiseDynamicList [] blist _ =
    ([], blist)
processPairwiseDynamicList alist [] _ =
    (alist, [])
processPairwiseDynamicList (a : as) (b : bs) fn = do
    let (a', b') = fn a b
    let (a'', bs') = processPairwiseDynamicList a' bs fn
    let (as', blist') = processPairwiseDynamicList as (b' ++ bs') fn
    (a'' ++ as', blist')

data DataBox a = DataBox BBox BBox a
    deriving (Show, Eq, Ord)

instance HasBBox (DataBox a) where
    bboxOf (DataBox box _ _) = box

splitDataBox1 ::
    DataBox a ->
    DataBox b ->
    [DataBox a]
splitDataBox1
    (DataBox abox@(BBox axi ayi azi) abox2 adata)
    (DataBox _ bbox2@(BBox bxi byi bzi) _)
    | overlaps abox bbox2 = do
        xi2 <- splitInterval2 bxi axi
        yi2 <- splitInterval2 byi ayi
        zi2 <- splitInterval2 bzi azi
        [DataBox (BBox xi2 yi2 zi2) abox2 adata]
  where
    splitInterval2 (Interval c d) =
        splitInterval1 c >=> splitInterval1 d
    splitInterval1 x (Interval a b)
        | a < x && x < b =
            [Interval a x, Interval x b]
        | otherwise =
            [Interval a b]
splitDataBox1 a _ = [a]

splitDataBox ::
    DataBox a ->
    DataBox b ->
    ([DataBox a], [DataBox b])
splitDataBox a b =
    (splitDataBox1 a b, splitDataBox1 b a)

splitDataBoxRTree ::
    DataBox a ->
    DataBox b ->
    (RTree.RTree (DataBox a), RTree.RTree (DataBox b))
splitDataBoxRTree a b =
    (RTree.fromList $ splitDataBox1 a b, RTree.fromList $ splitDataBox1 b a)

-- mkBBox :: Vec3 -> Vec3 -> BBox
-- mkBBox (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    -- BBox
        -- (Interval x1 x2)
        -- (Interval y1 y2)
        -- (Interval z1 z2)

-- aboxes :: [DataBox Int]
-- aboxes =
    -- [ DataBox (mkBBox (Vec3 x 0 0) (Vec3 (x + 10) 10 10)) y
    -- | (x, y) <- zip [0, 10..] [1..10]
    -- ]

-- bboxes :: [DataBox Char]
-- bboxes =
    -- [ DataBox (mkBBox (Vec3 x 2 1) (Vec3 (x + 10) 12 11)) y
    -- | (x, y) <- zip [4, 14..] ['a'..'n']
    -- ]

-- abSplitReference :: (Set.Set (DataBox Int), Set.Set (DataBox Char))
-- abSplitReference = do
    -- let (as, bs) =
            -- processPairwiseDynamicList
                -- aboxes
                -- bboxes
                -- splitDataBox
    -- (Set.fromList as, Set.fromList bs)

-- abSplitRTree :: (Set.Set (DataBox Int), Set.Set (DataBox Char))
-- abSplitRTree = do
    -- let (as, bs) =
            -- RTree.processPairwiseDynamic
                -- (RTree.fromList aboxes)
                -- (RTree.fromList bboxes)
                -- splitDataBoxRTree
    -- (Set.fromList $ toList as, Set.fromList $ toList bs)

instance (Arbitrary a, Ord a) => Arbitrary (Interval a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        if x <= y
            then pure $ Interval x y
            else pure $ Interval y x

instance Arbitrary BBox where
    arbitrary = do
        w <- arbitrary @Word
        if w == 0
            then pure NullBBox
            else BBox
                <$> fmap (fmap fromInteger) arbitrary
                <*> fmap (fmap fromInteger) arbitrary
                -- <*> fmap (fmap fromInteger) arbitrary
                <*> pure (Interval 0 0)

instance (Arbitrary a) => Arbitrary (DataBox a) where
    arbitrary = do
        box <- arbitrary
        DataBox box box <$> arbitrary

newtype LongerList a = LongerList [a]
    deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (LongerList a) where
    arbitrary = do
        sized $ \n -> do
            k <- chooseInt (0, n*3)
            LongerList <$> vectorOf k arbitrary
    shrink (LongerList xs) =
        LongerList <$> shrink xs

spec :: Spec
spec = do
    describe "Geometry.RTree" $ do
        describe "processPairwiseDynamic" $ do
            -- it "static" $ do
                -- shouldBe abSplitRTree abSplitReference
            prop "random" $ \(LongerList raboxes, LongerList rbboxes) -> do
                let _ = raboxes :: [DataBox Int]
                let _ = rbboxes :: [DataBox Int]
                let rabSplitReference = do
                        let (as, bs) =
                                processPairwiseDynamicList
                                    raboxes
                                    rbboxes
                                    splitDataBox
                        (Set.fromList as, Set.fromList bs)
                let rabSplitRTree = do
                        let (as, bs) =
                                RTree.processPairwiseDynamic
                                    (RTree.fromList raboxes)
                                    (RTree.fromList rbboxes)
                                    splitDataBoxRTree
                        (Set.fromList $ toList as, Set.fromList $ toList bs)
                shouldBe rabSplitRTree rabSplitReference


{-
[ DataBox (BBox (Interval 3 5) (Interval (-4) 3) (Interval 0 0)) (-1)
, DataBox (BBox (Interval 2 3) (Interval (-3) 0) (Interval 0 0)) (-2)
]
[ DataBox (BBox (Interval 3 5) (Interval (-1) 1) (Interval 0 0)) (-5)
]



[ DataBox (BBox (Interval 2 3) (Interval (-3) (-1)) (Interval 0 0)) (-2)
, DataBox (BBox (Interval 2 3) (Interval (-1) 0) (Interval 0 0)) (-2)
, DataBox (BBox (Interval 3 5) (Interval (-4) (-1)) (Interval 0 0)) (-1)
, DataBox (BBox (Interval 3 5) (Interval (-1) 1) (Interval 0 0)) (-1)
, DataBox (BBox (Interval 3 5) (Interval 1 3) (Interval 0 0)) (-1)
]

[ DataBox (BBox (Interval 3 5) (Interval (-1) 0) (Interval 0 0)) (-5)
, DataBox (BBox (Interval 3 5) (Interval 0 1) (Interval 0 0)) (-5)
]



[ DataBox (BBox (Interval 2 3) (Interval (-3) (-1)) (Interval 0 0)) (-2)
, DataBox (BBox (Interval 2 3) (Interval (-1) 0) (Interval 0 0)) (-2)
, DataBox (BBox (Interval 3 5) (Interval (-4) (-1)) (Interval 0 0)) (-1)
, DataBox (BBox (Interval 3 5) (Interval (-1) 0) (Interval 0 0)) (-1)
, DataBox (BBox (Interval 3 5) (Interval 0 1) (Interval 0 0)) (-1)
, DataBox (BBox (Interval 3 5) (Interval 1 3) (Interval 0 0)) (-1)
]

[ DataBox (BBox (Interval 3 5) (Interval (-1) 0) (Interval 0 0)) (-5)
, DataBox (BBox (Interval 3 5) (Interval 0 1) (Interval 0 0)) (-5)
]
-}