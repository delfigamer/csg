module Geometry.Vec2 where

import Data.Interval
import Data.Ratio
import Geometry.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

type instance Coord IVec2 = Integer

data IVec2 = IVec2 !Integer !Integer
  deriving (Show, Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''IVec2

instance Vector IVec2 where
    IVec2 ax ay +. IVec2 bx by = IVec2 (ax + bx) (ay + by)
    IVec2 ax ay -. IVec2 bx by = IVec2 (ax - bx) (ay - by)
    a *. IVec2 bx by = IVec2 (a * bx) (a * by)
    negateVec (IVec2 ax ay) = IVec2 (-ax) (-ay)

instance Dot IVec2 where
    dot (IVec2 ax ay) (IVec2 bx by) = ax*bx + ay*by

type instance Coord Vec2 = Rational

data Vec2 = Vec2
    {-# UNPACK #-} !Rational
    {-# UNPACK #-} !Rational
  deriving (Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''Vec2

instance Show Vec2 where
    showsPrec d (Vec2 x y) =
        showParen (d > 10) $
        showString "Vec2 " .
        showsPrec 11 (fromRational x :: Double) .
        showString " " .
        showsPrec 11 (fromRational y :: Double)

instance Vector Vec2 where
    Vec2 ax ay +. Vec2 bx by = Vec2 (ax + bx) (ay + by)
    Vec2 ax ay -. Vec2 bx by = Vec2 (ax - bx) (ay - by)
    a *. Vec2 bx by = Vec2 (a * bx) (a * by)
    negateVec (Vec2 ax ay) = Vec2 (-ax) (-ay)

instance Dot Vec2 where
    dot (Vec2 ax ay) (Vec2 bx by) = ax*bx + ay*by

instance ToWolf Vec2 where
    toWolf (Vec2 x y) = wolfList [toWolf x, toWolf y]

vec2FromInteger :: IVec2 -> Vec2
vec2FromInteger (IVec2 x y) =
    Vec2 (fromInteger x) (fromInteger y)

vec2ToIntegerScaled :: Vec2 -> IVec2
vec2ToIntegerScaled (Vec2 rx ry) = do
    let ix = numerator rx * denominator ry
    let iy = denominator rx * numerator ry
    normalizeVec2i (IVec2 ix iy)

vec2Rational :: IVec2 -> Integer -> Vec2
vec2Rational (IVec2 x y) w =
    Vec2 (x % w) (y % w)

normalizeVec2i :: IVec2 -> IVec2
normalizeVec2i (IVec2 ix iy) = do
    let cden = ix `gcd` iy
    if cden == 0
        then IVec2 0 0
        else IVec2 (ix `quot` cden) (iy `quot` cden)

det2i :: IVec2 -> IVec2 -> Integer
det2i (IVec2 ax ay) (IVec2 bx by) = ax * by - ay * bx

det2 :: Vec2 -> Vec2 -> Rational
det2 (Vec2 ax ay) (Vec2 bx by) = ax * by - ay * bx

complexMul2i :: IVec2 -> IVec2 -> IVec2
complexMul2i (IVec2 ax ay) (IVec2 bx by) =
    normalizeVec2i $ IVec2 (ax * bx - ay * by) (ax * by + ay * bx)

complexConj2i :: IVec2 -> IVec2
complexConj2i (IVec2 ax ay) =
    IVec2 ax (-ay)

cross2i :: IVec2 -> IVec2
cross2i (IVec2 x y) = IVec2 (-y) x

cross2 :: Vec2 -> Vec2
cross2 (Vec2 x y) = Vec2 (-y) x

vec2ScalarDivide :: Vec2 -> Vec2 -> Maybe Rational
vec2ScalarDivide (Vec2 ax ay) (Vec2 bx by) = searchX
  where
    searchX
        | bx /= 0 = checkY (ax / bx)
        | ax /= 0 = Nothing
        | otherwise = searchY
    searchY
        | by /= 0 = Just (ay / by)
        | ay /= 0 = Nothing
        | otherwise = Just 0
    checkY k
        | ay == by * k = Just k
        | otherwise = Nothing

data Line2 = Line2
    { line2Origin :: !Vec2
    , line2Direction :: !Vec2
    }
  deriving (Eq, Ord)

instance ToWolf Line2 where
    toWolf (Line2 pt dir) = wolfApp "Line" [wolfList [toWolf a, toWolf b]]
      where
        a = pt +. 0.05 *. dir
        b = pt +. 0.95 *. dir

instance Show Line2 where
    showsPrec d (Line2 pt dir) =
        showParen (d > 10) $
        showString "line2Between " .
        showsPrec 11 pt .
        showString " " .
        showsPrec 11 (pt +. dir)

line2Between :: Vec2 -> Vec2 -> Line2
line2Between a b = Line2 a (b -. a)

data HalfPlane = HalfPlane
    { halfPlaneNormal :: {-# UNPACK #-} !IVec2
    , halfPlaneOffset :: !Integer
    }
  deriving (Show, Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''HalfPlane

line2ClipWithHalfPlane ::
    Line2 ->
    HalfPlane ->
    Interval (Extended Rational)
line2ClipWithHalfPlane (Line2 pt dir) hp@(HalfPlane ai aw) = do
    let a = vec2FromInteger ai
    let num = - dot a pt - fromInteger aw
    let det = dot a dir
    if
        | det > 0 -> intervalFrom (Interior (num / det))
        | det < 0 -> intervalTo (Interior (num / det))
        | homdot2 hp pt >= 0 -> fullSet
        | otherwise -> emptySet

inverseHalfPlane :: HalfPlane -> HalfPlane
inverseHalfPlane (HalfPlane ai aw) =
    HalfPlane (negateVec ai) (- aw)

homdot2 :: HalfPlane -> Vec2 -> Integer
homdot2 (HalfPlane (IVec2 px py) pw) (Vec2 ax ay) = do
    let qx = px * numerator ax * denominator ay
    let qy = py * denominator ax * numerator ay
    let qw = pw * denominator ax * denominator ay
    qx + qy + qw

halfPlane :: Integer -> Integer -> Integer -> HalfPlane
halfPlane ix iy iw = do
    let cden = ix `gcd` iy `gcd` iw
    if cden == 0
        then HalfPlane (IVec2 0 0) 0
        else
            HalfPlane
                (IVec2 (ix `quot` cden) (iy `quot` cden))
                (iw `quot` cden)

halfPlaneFromNormalAndOffset :: Vec2 -> Rational -> HalfPlane
halfPlaneFromNormalAndOffset (Vec2 rx ry) rw = do
    let ix = numerator rx * denominator ry * denominator rw
    let iy = denominator rx * numerator ry * denominator rw
    let iw = - denominator rx * denominator ry * numerator rw
    halfPlane ix iy iw

leftHalfPlane :: Vec2 -> Vec2 -> HalfPlane
leftHalfPlane a b = do
    let n = cross2 (b -. a)
    halfPlaneFromNormalAndOffset n (dot a n)

type instance Coord XRay2 = Rational

newtype XRay2 = XRay2 Rational
  deriving (Eq, Ord)

instance Show XRay2 where
    showsPrec d (XRay2 y) =
        showParen (d > 10) $
        showString "XRay2 " .
        showsPrec 11 (fromRational y :: Double)

instance Vector XRay2 where
    XRay2 ay +. XRay2 by = XRay2 (ay + by)
    XRay2 ay -. XRay2 by = XRay2 (ay - by)
    a *. XRay2 by = XRay2 (a * by)
    negateVec (XRay2 ay) = XRay2 (-ay)

xray2At :: Vec2 -> XRay2
xray2At (Vec2 _ y) = XRay2 y

data XRay2Half
    = XRay2Left
    | XRay2Right
    deriving (Show)

data XRayHalfPlaneIntersection
    = XRayHalfPlaneIntersectionPoint !XRay2Half {-# UNPACK #-} !Rational
    | XRayHalfPlaneIntersectionInside
    | XRayHalfPlaneIntersectionFront
    | XRayHalfPlaneIntersectionBack
    deriving (Show)

intersectXRayWithHalfPlane ::
    XRay2 ->
    HalfPlane ->
    XRayHalfPlaneIntersection
intersectXRayWithHalfPlane (XRay2 oy) (HalfPlane ai@(IVec2 aix aiy) aiw)
    | aix == 0 = do
        let by = aiy * numerator oy
        let bw = aiw * denominator oy
        let b = by + bw
        if
            | b > 0 -> XRayHalfPlaneIntersectionFront
            | b < 0 -> XRayHalfPlaneIntersectionBack
            | otherwise -> XRayHalfPlaneIntersectionInside
    | otherwise = do
        let Vec2 ax ay = vec2FromInteger ai
        let x = - (ay * oy + fromInteger aiw) / ax
        if ax >= 0
            then XRayHalfPlaneIntersectionPoint XRay2Right x
            else XRayHalfPlaneIntersectionPoint XRay2Left x

data Axis2 = Axis2
    { axis2Origin :: !Vec2
    , axis2Direction :: !IVec2
    , axis2DirectionSqr :: !Integer
    }
    deriving (Eq, Ord)

instance ToWolf Axis2 where
    toWolf (Axis2 pt idir _) = wolfApp "Line" [wolfList [toWolf a, toWolf b]]
      where
        a = pt +. 0.05 *. vec2FromInteger idir
        b = pt +. 0.95 *. vec2FromInteger idir

instance Show Axis2 where
    showsPrec d (Axis2 pt idir idirsqr) =
        showParen (d > 10) $
        showString "Axis2 " .
        showsPrec 11 pt .
        showString " " .
        showsPrec 11 idir .
        showString " " .
        showsPrec 11 idirsqr

makeAxis2Between :: Vec2 -> Vec2 -> (Bool, Axis2)
makeAxis2Between a b = do
    let d = b -. a
    let o = a -. (dot a d / dotSqr d) *. d
    let di = vec2ToIntegerScaled d
    let disqr = dotSqr di
    if di >= IVec2 0 0
        then (False, Axis2 o di disqr)
        else (True, Axis2 o (negateVec di) disqr)

axis2Dot :: Axis2 -> Vec2 -> Rational
axis2Dot (Axis2 _ idir _) v =
    dot (vec2FromInteger idir) v

axis2Point :: Axis2 -> Rational -> Vec2
axis2Point (Axis2 pt idir idirsqr) a =
    pt +. a *. vec2Rational idir idirsqr
