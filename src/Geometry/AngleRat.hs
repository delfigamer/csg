module Geometry.AngleRat where

import Control.Exception
import Data.Ratio
import Geometry.Class
import Geometry.Vec2
import Geometry.Vec3

data AngleRat
    = AngleRatPosX
    | AngleRatPosY {-# UNPACK #-} !Rational
    | AngleRatNegX
    | AngleRatNegY {-# UNPACK #-} !Rational
  deriving (Show, Eq, Ord)

negateAngleRat :: AngleRat -> AngleRat
negateAngleRat = \case
    AngleRatPosX -> AngleRatNegX
    AngleRatPosY t -> AngleRatNegY t
    AngleRatNegX -> AngleRatPosX
    AngleRatNegY t -> AngleRatPosY t

angleRatFromComplex :: Rational -> Rational -> AngleRat
angleRatFromComplex rx ry =
    if ry == 0
        then if rx >= 0
            then AngleRatPosX
            else AngleRatNegX
        else if ry > 0
            then AngleRatPosY (- rx / ry)
            else AngleRatNegY (- rx / ry)

angleRatFromComplexInt :: Integer -> Integer -> AngleRat
angleRatFromComplexInt rx ry =
    if ry == 0
        then if rx >= 0
            then AngleRatPosX
            else AngleRatNegX
        else if ry > 0
            then AngleRatPosY (- rx % ry)
            else AngleRatNegY (- rx % ry)

angleRat2 :: Vec2 -> Vec2 -> AngleRat
angleRat2 a b =
    angleRatFromComplex (dot a b) (det2 a b)

{-  Given vectors N, A and B, such that N is orthogonal to both A and B,
and the angle between A and B is α (0 ≼ α < 2 Pi), it calculates a
characteristic ch[N, α], such that, if α < β, then ch[N, α] < ch[N, β].
That is, provided we use the same normal N and base vector A, sorting multiple
vectors according to this characteristic will yield a counter-clockwise order,
as seen from the end of N, starting from A.
    More specifically:
    1. We calculate a dot product:
        x = dot A B == abs A * abs B * cos α
    2. We calculate a 3-way wedge product:
        y = det3 N A B == abs A * abs B * abs N * sin α
    3. We construct an equivalent representation of this expression:
        θ = atan2 y x = atan2 (abs N * sin α) (cos α)
    The additional factor of (abs N) is equivalent to stretching the entire
AB-plane in the direction of `cross N A` by the length of vector N. While
this makes the calculated angle θ different from the actual α, it doesn't
bother us - our real objective is to sort points in a counter-clockwise order,
with no regard to the actual values of their polar angles. And, as long as we
use the same vector N for all the points in the set - sorting by θ indeed gives
the same answer as sorting by α. -}
angleRat3 :: Vec3 -> Vec3 -> Vec3 -> AngleRat
angleRat3 n a b =
    assert (dot n a == 0 && dot n b == 0) $ do
        angleRatFromComplex (dot a b) (det3 n a b)

angleRat3i :: IVec3 -> IVec3 -> IVec3 -> AngleRat
angleRat3i n a b =
    assert (dot n a == 0 && dot n b == 0) $ do
        angleRatFromComplexInt (dot a b) (det3i n a b)
