module Geometry.Vec3 where

import Data.Interval
import Data.Ratio
import Geometry.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

type instance Coord IVec3 = Integer

data IVec3 = IVec3 !Integer !Integer !Integer
    deriving (Show, Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''IVec3

instance Vector IVec3 where
    IVec3 ax ay az +. IVec3 bx by bz = IVec3 (ax + bx) (ay + by) (az + bz)
    IVec3 ax ay az -. IVec3 bx by bz = IVec3 (ax - bx) (ay - by) (az - bz)
    a *. IVec3 bx by bz = IVec3 (a * bx) (a * by) (a * bz)
    negateVec (IVec3 ax ay az) = IVec3 (-ax) (-ay) (-az)

instance Dot IVec3 where
    dot (IVec3 ax ay az) (IVec3 bx by bz) = ax*bx + ay*by + az*bz

type instance Coord Vec3 = Rational

data Vec3 = Vec3
    {-# UNPACK #-} !Rational
    {-# UNPACK #-} !Rational
    {-# UNPACK #-} !Rational
    deriving (Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''Vec3

instance Show Vec3 where
    showsPrec d (Vec3 x y z) =
        showParen (d > 10) $
        showString "Vec3 " .
        showsPrec 11 (fromRational x :: Double) .
        showString " " .
        showsPrec 11 (fromRational y :: Double) .
        showString " " .
        showsPrec 11 (fromRational z :: Double)

instance Vector Vec3 where
    Vec3 ax ay az +. Vec3 bx by bz = Vec3 (ax + bx) (ay + by) (az + bz)
    Vec3 ax ay az -. Vec3 bx by bz = Vec3 (ax - bx) (ay - by) (az - bz)
    a *. Vec3 bx by bz = Vec3 (a * bx) (a * by) (a * bz)
    negateVec (Vec3 ax ay az) = Vec3 (-ax) (-ay) (-az)

instance Dot Vec3 where
    dot (Vec3 ax ay az) (Vec3 bx by bz) = ax*bx + ay*by + az*bz

instance ToWolf Vec3 where
    toWolf (Vec3 x y z) = wolfList [toWolf x, toWolf y, toWolf z]

vec3FromInteger :: IVec3 -> Vec3
vec3FromInteger (IVec3 x y z) =
    Vec3 (fromInteger x) (fromInteger y) (fromInteger z)

vec3ToIntegerScaled :: Vec3 -> IVec3
vec3ToIntegerScaled (Vec3 rx ry rz) = do
    let ix = numerator rx * denominator ry * denominator rz
    let iy = denominator rx * numerator ry * denominator rz
    let iz = denominator rx * denominator ry * numerator rz
    normalizeVec3i (IVec3 ix iy iz)

vec3Rational :: IVec3 -> Integer -> Vec3
vec3Rational (IVec3 x y z) w =
    Vec3 (x % w) (y % w) (z % w)

normalizeVec3i :: IVec3 -> IVec3
normalizeVec3i (IVec3 ix iy iz) = do
    let cden = ix `gcd` iy `gcd` iz
    if cden == 0
        then IVec3 0 0 0
        else IVec3 (ix `quot` cden) (iy `quot` cden) (iz `quot` cden)

cross3i :: IVec3 -> IVec3 -> IVec3
cross3i (IVec3 ax ay az) (IVec3 bx by bz) =
    IVec3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

cross3 :: Vec3 -> Vec3 -> Vec3
cross3 (Vec3 ax ay az) (Vec3 bx by bz) =
    Vec3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

det3i :: IVec3 -> IVec3 -> IVec3 -> Integer
det3i a b c = dot a (cross3i b c)

det3 :: Vec3 -> Vec3 -> Vec3 -> Rational
det3 a b c = dot a (cross3 b c)

vec3ScalarDivide :: Vec3 -> Vec3 -> Maybe Rational
vec3ScalarDivide (Vec3 ax ay az) (Vec3 bx by bz) = searchX
  where
    searchX
        | bx /= 0 = checkY (ax / bx)
        | ax /= 0 = Nothing
        | otherwise = searchY
    searchY
        | by /= 0 = checkZ (ay / by)
        | ay /= 0 = Nothing
        | otherwise = searchZ
    searchZ
        | bz /= 0 = Just (az / bz)
        | az /= 0 = Nothing
        | otherwise = Just 0
    checkY k
        | ay == by * k = checkZ k
        | otherwise = Nothing
    checkZ k
        | az == bz * k = Just k
        | otherwise = Nothing

squareXCos3i :: IVec3 -> Rational
squareXCos3i (IVec3 x y z) = x*x % (x*x + y*y + z*z)

data HalfSpace = HalfSpace
    { halfSpaceNormal :: {-# UNPACK #-} !IVec3
    , halfSpaceOffset :: !Integer
    }
    deriving (Show, Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''HalfSpace

inverseHalfSpace :: HalfSpace -> HalfSpace
inverseHalfSpace (HalfSpace ai aw) =
    HalfSpace (negateVec ai) (- aw)

homdot3 :: HalfSpace -> Vec3 -> Integer
homdot3 (HalfSpace (IVec3 px py pz) pw) (Vec3 ax ay az) = do
    let qx = px * numerator ax * denominator ay * denominator az
    let qy = py * denominator ax * numerator ay * denominator az
    let qz = pz * denominator ax * denominator ay * numerator az
    let qw = pw * denominator ax * denominator ay * denominator az
    qx + qy + qz + qw

halfSpaceFromNormalAndOffset :: Vec3 -> Rational -> HalfSpace
halfSpaceFromNormalAndOffset (Vec3 rx ry rz) rw = do
    let ix = numerator rx * denominator ry * denominator rz * denominator rw
    let iy = denominator rx * numerator ry * denominator rz * denominator rw
    let iz = denominator rx * denominator ry * numerator rz * denominator rw
    let iw = - denominator rx * denominator ry * denominator rz * numerator rw
    let cden = ix `gcd` iy `gcd` iz `gcd` iw
    if cden == 0
        then HalfSpace (IVec3 0 0 0) 0
        else
            HalfSpace
                (IVec3 (ix `quot` cden) (iy `quot` cden) (iz `quot` cden))
                (iw `quot` cden)

halfSpaceFromNormalAndOrigin :: Vec3 -> Vec3 -> HalfSpace
halfSpaceFromNormalAndOrigin r o =
    halfSpaceFromNormalAndOffset r (dot o r)

data Segment3 = Segment3 !Vec3 !Vec3
    deriving (Eq, Ord)

instance ToWolf Segment3 where
    toWolf (Segment3 a b) = toWolf (line3Between a b)

lineToSegment3 :: Line3 -> Segment3
lineToSegment3 (Line3 pt dir) = Segment3 pt (pt +. dir)

segmentToLine3 :: Segment3 -> Line3
segmentToLine3 (Segment3 a b) = Line3 a (b -. a)

data Line3 = Line3
    { line3Origin :: !Vec3
    , line3Direction :: !Vec3
    }
    deriving (Eq, Ord)

instance ToWolf Line3 where
    toWolf (Line3 pt dir) = wolfApp "Line" [wolfList [toWolf a, toWolf b]]
      where
        a = pt +. 0.05 *. dir
        b = pt +. 0.95 *. dir

instance Show Line3 where
    showsPrec d (Line3 pt dir) =
        showParen (d > 10) $
        showString "line3Between " .
        showsPrec 11 pt .
        showString " " .
        showsPrec 11 (pt +. dir)

line3Between :: Vec3 -> Vec3 -> Line3
line3Between a b = Line3 a (b -. a)

line3ClipWithHalfSpace ::
    Line3 ->
    HalfSpace ->
    Interval (Extended Rational)
line3ClipWithHalfSpace (Line3 pt dir) hs@(HalfSpace ai aw) = do
    let a = vec3FromInteger ai
    let num = - dot a pt - fromInteger aw
    let det = dot a dir
    if
        | det > 0 -> intervalFrom (Interior (num / det))
        | det < 0 -> intervalTo (Interior (num / det))
        | homdot3 hs pt >= 0 -> fullSet
        | otherwise -> emptySet

data HalfSpaceRelation
    = HalfSpaceCoplanar Bool
    | HalfSpaceParallel
    | HalfSpaceIntersect Line3
    deriving (Show, Eq, Ord)

halfSpaceRelation :: HalfSpace -> HalfSpace -> HalfSpaceRelation
halfSpaceRelation (HalfSpace ai aw) (HalfSpace bi bw)
    | (ai, aw) == (bi, bw) =
        HalfSpaceCoplanar True
    | (ai, aw) == (negateVec bi, -bw) =
        HalfSpaceCoplanar False
    | ni == IVec3 0 0 0 =
        HalfSpaceParallel
    | otherwise = do
        let dir = vec3FromInteger ni
        let ndet = dotSqr ni
        let ui = cross3i (bw *. ai -. aw *. bi) ni
        let pt = vec3Rational ui ndet
        HalfSpaceIntersect (Line3 pt dir)
  where
    ni = cross3i ai bi

type instance Coord XRay3 = Rational

data XRay3 = XRay3
    {-# UNPACK #-} !Rational
    {-# UNPACK #-} !Rational
    deriving (Eq, Ord)

instance Show XRay3 where
    showsPrec d (XRay3 y z) =
        showParen (d > 10) $
        showString "XRay3 " .
        showsPrec 11 (fromRational y :: Double) .
        showString " " .
        showsPrec 11 (fromRational z :: Double)

instance Vector XRay3 where
    XRay3 ay az +. XRay3 by bz = XRay3 (ay + by) (az + bz)
    XRay3 ay az -. XRay3 by bz = XRay3 (ay - by) (az - bz)
    a *. XRay3 by bz = XRay3 (a * by) (a * bz)
    negateVec (XRay3 ay az) = XRay3 (-ay) (-az)

xray3At :: Vec3 -> XRay3
xray3At (Vec3 _ y z) = XRay3 y z

data XRay3Half
    = XRay3Left
    | XRay3Right
    deriving (Show, Eq)

data XRayHalfSpaceIntersection
    = XRayHalfSpaceIntersectionPoint !XRay3Half {-# UNPACK #-} !Rational
    | XRayHalfSpaceIntersectionInside
    | XRayHalfSpaceIntersectionFront
    | XRayHalfSpaceIntersectionBack
    deriving (Show, Eq)

intersectXRayWithHalfSpace ::
    XRay3 ->
    HalfSpace ->
    XRayHalfSpaceIntersection
intersectXRayWithHalfSpace (XRay3 oy oz) (HalfSpace ai@(IVec3 aix aiy aiz) aiw)
    | aix == 0 = do
        let by = aiy * numerator oy * denominator oz
        let bz = aiz * denominator oy * numerator oz
        let bw = aiw * denominator oy * denominator oz
        let b = by + bz + bw
        if
            | b > 0 -> XRayHalfSpaceIntersectionFront
            | b < 0 -> XRayHalfSpaceIntersectionBack
            | otherwise -> XRayHalfSpaceIntersectionInside
    | otherwise = do
        let Vec3 ax ay az = vec3FromInteger ai
        let x = - (ay * oy + az * oz + fromInteger aiw) / ax
        if ax >= 0
            then XRayHalfSpaceIntersectionPoint XRay3Right x
            else XRayHalfSpaceIntersectionPoint XRay3Left x

-- clipXRayWithHalfSpace ::
    -- XRay ->
    -- HalfSpace ->
    -- Interval (Extended Rational)
-- clipXRayWithHalfSpace (XRay oy oz) (HalfSpace ai@(IVec3 aix aiy aiz) aiw)
    -- | aix == 0 = do
        -- let by = aiy * numerator oy * denominator oz
        -- let bz = aiz * denominator oy * numerator oz
        -- let bw = aiw * denominator oy * denominator oz
        -- if by + bz + bw >= 0
            -- then fullSet
            -- else emptySet
    -- | otherwise = do
        -- let Vec3 ax ay az = vec3FromInteger ai
        -- let x = - (ay * oy + az * oz + fromInteger aiw) / ax
        -- if ax >= 0
            -- then intervalFrom (Interior x)
            -- else intervalTo (Interior x)

-- intersectXRayWithHalfSpace ::
    -- XRay ->
    -- HalfSpace ->
    -- Rational
-- intersectXRayWithHalfSpace (XRay oy oz) (HalfSpace ai aiw) = do
    -- let Vec3 ax ay az = vec3FromInteger ai
    -- - (ay * oy + az * oz + fromInteger aiw) / ax

data Axis3 = Axis3
    { axis3Origin :: !Vec3
    , axis3Direction :: !IVec3
    , axis3DirectionSqr :: !Integer
    }
    deriving (Eq, Ord)

instance ToWolf Axis3 where
    toWolf (Axis3 pt idir _) = wolfApp "Line" [wolfList [toWolf a, toWolf b]]
      where
        a = pt +. 0.05 *. vec3FromInteger idir
        b = pt +. 0.95 *. vec3FromInteger idir

instance Show Axis3 where
    showsPrec d (Axis3 pt idir _) =
        showParen (d > 10) $
        showString "Axis3 " .
        showsPrec 11 pt .
        showString " " .
        showsPrec 11 idir

makeAxis3Between :: Vec3 -> Vec3 -> (Bool, Axis3)
makeAxis3Between a b = do
    let d = b -. a
    let o = a -. (dot a d / dotSqr d) *. d
    let di = vec3ToIntegerScaled d
    let disqr = dotSqr di
    if di >= IVec3 0 0 0
        then (False, Axis3 o di disqr)
        else (True, Axis3 o (negateVec di) disqr)

axis3Dot :: Axis3 -> Vec3 -> Rational
axis3Dot (Axis3 _ idir _) v =
    dot (vec3FromInteger idir) v

axis3Point :: Axis3 -> Rational -> Vec3
axis3Point (Axis3 pt idir idirsqr) a =
    pt +. a *. vec3Rational idir idirsqr

{-  Given a vector V, produces an arbitrary, but deterministic, vector N,
    such that (dot V N == 0) -}
orthogonalVec3i :: IVec3 -> IVec3
orthogonalVec3i (IVec3 xi yi zi)
    | zi == 0 = do
        IVec3 yi (-xi) 0
    | otherwise =
        IVec3 0 zi (-yi)
