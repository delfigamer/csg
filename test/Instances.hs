{-# OPTIONS_GHC -Wno-orphans #-}

module Instances where

import Data.Interval
import Geometry.BBox
import Test.QuickCheck

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

newtype LongerList a = LongerList [a]
    deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (LongerList a) where
    arbitrary = do
        sized $ \n -> do
            k <- chooseInt (0, n*3)
            LongerList <$> vectorOf k arbitrary
    shrink (LongerList xs) =
        LongerList <$> shrink xs
