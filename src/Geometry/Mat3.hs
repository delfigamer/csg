module Geometry.Mat3 where

import Control.Monad
import Geometry.Class
import Geometry.Vec3
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

type instance Coord Mat3 = Rational

{- column-major -}
data Mat3 = Mat3Cols
    {-# UNPACK #-} !Vec3
    {-# UNPACK #-} !Vec3
    {-# UNPACK #-} !Vec3
  deriving (Show, Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''Mat3

pattern Mat3 ::
    Rational -> Rational -> Rational ->
    Rational -> Rational -> Rational ->
    Rational -> Rational -> Rational ->
    Mat3
pattern Mat3 xx xy xz yx yy yz zx zy zz =
    Mat3Cols (Vec3 xx yx zx) (Vec3 xy yy zy) (Vec3 xz yz zz)
{-# COMPLETE Mat3 #-}

instance Vector Mat3 where
    Mat3Cols ax ay az +. Mat3Cols bx by bz =
        Mat3Cols (ax +. bx) (ay +. by) (az +. bz)
    Mat3Cols ax ay az -. Mat3Cols bx by bz =
        Mat3Cols (ax -. bx) (ay -. by) (az -. bz)
    a *. Mat3Cols bx by bz =
        Mat3Cols (a *. bx) (a *. by) (a *. bz)
    negateVec (Mat3Cols ax ay az) =
        Mat3Cols (negateVec ax) (negateVec ay) (negateVec az)

mat3Transpose :: Mat3 -> Mat3
mat3Transpose (Mat3 xx xy xz yx yy yz zx zy zz) =
    Mat3 xx yx zx xy yy zy xz yz zz

mat3Inverse :: Mat3 -> Maybe Mat3
mat3Inverse (Mat3Cols ax ay az) = do
    let d = det3 ax ay az
    guard (d /= 0)
    let Vec3 xx yx zx = ax
    let Vec3 xy yy zy = ay
    let Vec3 xz yz zz = az
    let m = Mat3
            (yy * zz - yz * zy) (xz * zy - xy * zz) (xy * yz - xz * yy)
            (yz * zx - yx * zz) (xx * zz - xz * zx) (xz * yx - xx * yz)
            (yx * zy - yy * zx) (xy * zx - xx * zy) (xx * yy - xy * yx)
    Just (recip d *. m)

instance Inner Mat3 Mat3 where
    (*!) (Mat3Cols ax ay az) (Mat3 xx xy xz yx yy yz zx zy zz) =
        Mat3Cols
            (xx *. ax +. yx *. ay +. zx *. az)
            (xy *. ax +. yy *. ay +. zy *. az)
            (xz *. ax +. yz *. ay +. zz *. az)

instance Inner Mat3 Vec3 where
    (*!) (Mat3Cols ax ay az) (Vec3 x y z) =
        x *. ax +. y *. ay +. z *. az

pattern Mat3Identity :: Mat3
pattern Mat3Identity = Mat3 1 0 0 0 1 0 0 0 1

data Xform3 = Xform3
    { xformVec3 :: {-# UNPACK #-} !Vec3
    , xformMat3 :: {-# UNPACK #-} !Mat3
    }
  deriving (Show, Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''Xform3

xform3Inverse :: Xform3 -> Maybe Xform3
xform3Inverse (Xform3 vec mat) = do
    invmat <- mat3Inverse mat
    Just (Xform3 (invmat *! negateVec vec) invmat)

instance Inner Xform3 Xform3 where
    (*!) (Xform3 va ma) (Xform3 vb mb) =
        Xform3 (va +. ma *! vb) (ma *! mb)

instance Inner Xform3 Vec3 where
    (*!) (Xform3 va ma) vb =
        va +. ma *! vb

pattern Xform3Identity :: Xform3
pattern Xform3Identity = Xform3 (Vec3 0 0 0) Mat3Identity
