module Geometry.Mat2 where

import Control.Monad
import Geometry.Class
import Geometry.Vec2

type instance Coord Mat2 = Rational

{- column-major -}
data Mat2 = Mat2Cols
    {-# UNPACK #-} !Vec2
    {-# UNPACK #-} !Vec2
  deriving (Show, Eq, Ord)

{- row-major -}
pattern Mat2 ::
    Rational -> Rational ->
    Rational -> Rational ->
    Mat2
pattern Mat2 xx xy yx yy =
    Mat2Cols (Vec2 xx yx) (Vec2 xy yy)
{-# COMPLETE Mat2 #-}

instance Vector Mat2 where
    Mat2Cols ax ay +. Mat2Cols bx by =
        Mat2Cols (ax +. bx) (ay +. by)
    Mat2Cols ax ay -. Mat2Cols bx by =
        Mat2Cols (ax -. bx) (ay -. by)
    a *. Mat2Cols bx by =
        Mat2Cols (a *. bx) (a *. by)
    negateVec (Mat2Cols ax ay) =
        Mat2Cols (negateVec ax) (negateVec ay)

mat2Transpose :: Mat2 -> Mat2
mat2Transpose (Mat2 xx xy yx yy) =
    Mat2 xx yx xy yy

mat2Inverse :: Mat2 -> Maybe Mat2
mat2Inverse (Mat2Cols ax ay) = do
    let d = det2 ax ay
    guard (d /= 0)
    let Vec2 xx yx = ax
    let Vec2 xy yy = ay
    let m = Mat2
            ( yy) (-xy)
            (-yx) ( xx)
    Just (recip d *. m)

instance Inner Mat2 Mat2 where
    (*!) (Mat2Cols ax ay) (Mat2 xx xy yx yy) =
        Mat2Cols
            (xx *. ax +. yx *. ay)
            (xy *. ax +. yy *. ay)

instance Inner Mat2 Vec2 where
    (*!) (Mat2Cols ax ay) (Vec2 x y) =
        x *. ax +. y *. ay

pattern Mat2Identity :: Mat2
pattern Mat2Identity = Mat2 1 0 0 1

data Xform2 = Xform2
    { xformVec2 :: {-# UNPACK #-} !Vec2
    , xformMat2 :: {-# UNPACK #-} !Mat2
    }
  deriving (Show, Eq, Ord)

xform2Inverse :: Xform2 -> Maybe Xform2
xform2Inverse (Xform2 vec mat) = do
    invmat <- mat2Inverse mat
    Just (Xform2 (invmat *! negateVec vec) invmat)

instance Inner Xform2 Xform2 where
    (*!) (Xform2 va ma) (Xform2 vb mb) =
        Xform2 (va +. ma *! vb) (ma *! mb)

instance Inner Xform2 Vec2 where
    (*!) (Xform2 va ma) vb =
        va +. ma *! vb

pattern Xform2Identity :: Xform2
pattern Xform2Identity = Xform2 (Vec2 0 0) Mat2Identity
