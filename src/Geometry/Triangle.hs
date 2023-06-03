module Geometry.Triangle where

import Data.Sequence (Seq (..))
import Geometry.BBox
import Geometry.Vec3
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Sequence as Seq

data Triangle a = Triangle
    { triPoint1 :: Vec3
    , triPoint2 :: Vec3
    , triPoint3 :: Vec3
    , triBBox :: BBox
    , triData :: a
    }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

Aeson.deriveJSON Aeson.defaultOptions ''Triangle

instance (Show a) => Show (Triangle a) where
    showsPrec d tri =
        showParen (d > 10) $
            showString "makeTriangle " .
            showsPrec 11 (triPoint1 tri) .
            showString " " .
            showsPrec 11 (triPoint2 tri) .
            showString " " .
            showsPrec 11 (triPoint3 tri) .
            showString " " .
            showsPrec 11 (triData tri)

instance HasBBox (Triangle a) where
    bboxOf = triBBox

triPoints :: Triangle a -> Seq Vec3
triPoints tri =
    Seq.fromList
        [ triPoint1 tri
        , triPoint2 tri
        , triPoint3 tri
        ]

makeTriangle :: Vec3 -> Vec3 -> Vec3 -> a -> Triangle a
makeTriangle v1 v2 v3 d = do
    Triangle
        { triPoint1 = v1
        , triPoint2 = v2
        , triPoint3 = v3
        , triBBox = foldableBBox $ fmap pointBBox [v1, v2, v3]
        , triData = d
        }
