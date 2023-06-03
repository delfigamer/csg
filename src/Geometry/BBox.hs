module Geometry.BBox where

import Data.Interval
import Data.Foldable
import Geometry.Vec3
import qualified Data.Aeson as Aeson

data BBox
    = BBox
        {-# UNPACK #-} !(Interval Rational)
        {-# UNPACK #-} !(Interval Rational)
        {-# UNPACK #-} !(Interval Rational)
  deriving (Show, Eq, Ord)

bboxXInterval :: BBox -> Interval Rational
bboxXInterval (BBox xi _ _) = xi

bboxYInterval :: BBox -> Interval Rational
bboxYInterval (BBox _ yi _) = yi

bboxZInterval :: BBox -> Interval Rational
bboxZInterval (BBox _ _ zi) = zi

bboxDoubleCenter :: BBox -> Vec3
bboxDoubleCenter (BBox (Interval x1 x2) (Interval y1 y2) (Interval z1 z2)) =
    Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

instance Aeson.ToJSON BBox where
    toJSON (BBox xi yi zi) =
        Aeson.toJSON [xi, yi, zi]

instance Aeson.FromJSON BBox where
    parseJSON x = flip (Aeson.withArray "BBox") x $ \list ->
        case toList list of
            [xi, yi, zi] ->
                BBox
                    <$> Aeson.parseJSON xi
                    <*> Aeson.parseJSON yi
                    <*> Aeson.parseJSON zi
            _ ->
                fail "invalid BBox"

instance IsInterval BBox where
    intersect (BBox axr ayr azr) (BBox bxr byr bzr) =
        BBox
            (intersect axr bxr)
            (intersect ayr byr)
            (intersect azr bzr)
    hull (BBox axr ayr azr) (BBox bxr byr bzr) =
        BBox
            (hull axr bxr)
            (hull ayr byr)
            (hull azr bzr)
    isEmptySet (BBox xr yr zr) =
        isEmptySet xr || isEmptySet yr || isEmptySet zr

pointBBox :: Vec3 -> BBox
pointBBox (Vec3 x y z) =
    BBox
        (Interval x x)
        (Interval y y)
        (Interval z z)

bboxContainsXRay3 :: XRay3 -> BBox -> Bool
bboxContainsXRay3 (XRay3 y z) (BBox _ yr zr) =
    intervalContains y yr && intervalContains z zr

class HasBBox a where
    bboxOf :: a -> BBox

instance HasBBox BBox where
    bboxOf = id

instance HasBBox Vec3 where
    bboxOf = pointBBox

foldableBBox ::
    (Foldable f, HasBBox a) =>
    f a ->
    BBox
foldableBBox fa =
    case foldMap (Just . Hull . bboxOf) fa of
        Nothing -> error "empty container"
        Just (Hull bbox) -> bbox
