{-# LANGUAGE OverloadedLists #-}

module Geometry.PlaneSpaceSpec where

-- import Control.Exception
-- import Control.Monad
-- import Data.CList
-- import Data.Interval
-- import Data.Maybe
-- import Data.MinMaxWith
-- import Data.Monoid
-- import Data.NubList
-- import Data.Semigroup
-- import Geometry.Algorithm.SplitFace
import Data.Ratio
import Geometry.Class
import Geometry.PlaneSpace
import Geometry.Vec2
import Geometry.Vec3
import Test.Hspec

spec :: Spec
spec = do
    describe "Geometry.PlaneSpace" $ do
        it "planeSpaceFromHalfSpace" $ do
            shouldBe
                (planeSpaceFromHalfSpace $
                    halfSpaceFromNormalAndOrigin
                        (Vec3 1 0 0)
                        (Vec3 0 2 3)
                )
                (PlaneSpace
                    (IVec3 0 1 0)
                    1
                    (IVec3 0 0 1)
                    1
                    (HalfSpace (IVec3 1 0 0) 0)
                    (Vec3 0 0 0)
                    (Vec2 1 0)
                    (Vec2 0 1)
                )
            shouldBe
                (planeSpaceFromHalfSpace $
                    halfSpaceFromNormalAndOrigin
                        (Vec3 1 1 0)
                        (Vec3 2 3 4)
                )
                (PlaneSpace
                    (IVec3 1 (-1) 0)
                    2
                    (IVec3 0 0 (-1))
                    1
                    (HalfSpace (IVec3 1 1 0) (-5))
                    (Vec3 2.5 2.5 0)
                    (Vec2 (-1) 0)
                    (Vec2 0 (-1))
                )
            shouldBe
                (planeSpaceFromHalfSpace $
                    halfSpaceFromNormalAndOrigin
                        (Vec3 0 1 0)
                        (Vec3 2 3 4)
                )
                (PlaneSpace
                    (IVec3 1 0 0)
                    1
                    (IVec3 0 0 (-1))
                    1
                    (HalfSpace (IVec3 0 1 0) (-3))
                    (Vec3 0 3 0)
                    (Vec2 0 0)
                    (Vec2 0 0)
                )
        it "intersectXRayWithPlaneSpace" $ do
            shouldBe
                (intersectXRayWithPlaneSpace
                    (XRay3 0 0)
                    (planeSpaceFromHalfSpace $
                        halfSpaceFromNormalAndOrigin
                            (Vec3 1 0 0)
                            (Vec3 0 2 3)
                    )
                )
                (XRayPlaneSpaceIntersectionPoint XRay3Right 0 (Vec2 0 0))
            shouldBe
                (intersectXRayWithPlaneSpace
                    (XRay3 5 7)
                    (planeSpaceFromHalfSpace $
                        halfSpaceFromNormalAndOrigin
                            (Vec3 1 0 0)
                            (Vec3 0 2 3)
                    )
                )
                (XRayPlaneSpaceIntersectionPoint XRay3Right 0 (Vec2 5 7))
            shouldBe
                (intersectXRayWithPlaneSpace
                    (XRay3 5 7)
                    (planeSpaceFromHalfSpace $
                        halfSpaceFromNormalAndOrigin
                            (Vec3 1 1 0)
                            (Vec3 2 3 4)
                    )
                )
                (XRayPlaneSpaceIntersectionPoint XRay3Right 0 (Vec2 (-2.5) (-7)))
            shouldBe
                (intersectXRayWithPlaneSpace
                    (XRay3 7 11)
                    (planeSpaceFromHalfSpace $
                        halfSpaceFromNormalAndOrigin
                            (Vec3 (-1) 1 2)
                            (Vec3 2 3 4)
                    )
                )
                (XRayPlaneSpaceIntersectionPoint XRay3Left 20 (Vec2 4.3 0.6))
            shouldBe
                (intersectXRayWithPlaneSpace
                    (XRay3 7 11)
                    (planeSpaceFromHalfSpace $
                        halfSpaceFromNormalAndOrigin
                            (Vec3 0 1 2)
                            (Vec3 2 3 4)
                    )
                )
                XRayPlaneSpaceIntersectionFront
            shouldBe
                (intersectXRayWithPlaneSpace
                    (XRay3 7 2)
                    (planeSpaceFromHalfSpace $
                        halfSpaceFromNormalAndOrigin
                            (Vec3 0 1 2)
                            (Vec3 2 3 4)
                    )
                )
                (XRayPlaneSpaceIntersectionInside (XRay2 2.4))
