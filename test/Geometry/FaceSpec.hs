{-# LANGUAGE OverloadedLists #-}

module Geometry.FaceSpec where

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
import Geometry.Face
import Geometry.PlaneSpace
import Geometry.Vec2
import Geometry.Vec3
import Test.Hspec

spec :: Spec
spec = do
    describe "Geometry.Face" $ do
        describe "intersectXRayWithFace" $ do
            it "XY square" $ do
                let face =
                        makeFace
                            [ Vec3 0 0 0
                            , Vec3 4 0 0
                            , Vec3 4 4 0
                            , Vec3 0 4 0
                            ]
                            ()
                shouldBe
                    (intersectXRayWithFace (XRay3 0 1) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 0 (-1)) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 (-1) 0) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 5 0) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 0 0) face)
                    (Just $ XRayFaceIntersection 4 0 0 (-1))
                shouldBe
                    (intersectXRayWithFace (XRay3 2 0) face)
                    (Just $ XRayFaceIntersection 4 0 0 (-1))
                shouldBe
                    (intersectXRayWithFace (XRay3 4 0) face)
                    (Just $ XRayFaceIntersection 4 0 0 (-1))
            it "XY quad" $ do
                let face =
                        makeFace
                            [ Vec3 0 0 0
                            , Vec3 4 1 0
                            , Vec3 5 2 0
                            , Vec3 (-1) 4 0
                            ]
                            ()
                shouldBe
                    (intersectXRayWithFace (XRay3 0 1) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 0 (-1)) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 (-1) 0) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 5 0) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 0.0 0) face)
                    (Just $ XRayFaceIntersection 0 0 (16%17) (-1))
                shouldBe
                    (intersectXRayWithFace (XRay3 0.5 0) face)
                    (Just $ XRayFaceIntersection 2.0 (-0.125) (16%17) (-1))
                shouldBe
                    (intersectXRayWithFace (XRay3 1.0 0) face)
                    (Just $ XRayFaceIntersection 4.0 (-0.250) (1%2) (-1))
                shouldBe
                    (intersectXRayWithFace (XRay3 1.5 0) face)
                    (Just $ XRayFaceIntersection 4.5 (-0.375) (1%2) (-1))
                shouldBe
                    (intersectXRayWithFace (XRay3 2.0 0) face)
                    (Just $ XRayFaceIntersection 5.0 (-0.500) (-1%2) (-1))
                shouldBe
                    (intersectXRayWithFace (XRay3 3.0 0) face)
                    (Just $ XRayFaceIntersection 2.0 (-0.750) (9%10) (-1))
                shouldBe
                    (intersectXRayWithFace (XRay3 4.0 0) face)
                    (Just $ XRayFaceIntersection (-1.0) (-1.000) (9%10) (-1))
            it "XY triangle" $ do
                let face =
                        makeFace
                            [ Vec3 0 0 0
                            , Vec3 (-1) 1 0
                            , Vec3 (-2) 1 0
                            ]
                            ()
                shouldBe
                    (intersectXRayWithFace (XRay3 0 0) face)
                    (Just $ XRayFaceIntersection 0 0 (-1%2) (-1))
            it "YZ triangle" $ do
                let face =
                        makeFace
                            [ Vec3 0 0 0
                            , Vec3 0 1 0
                            , Vec3 0 0 1
                            ]
                            ()
                shouldBe
                    (intersectXRayWithFace (XRay3 0.0 0.0) face)
                    (Just $ XRayFaceIntersection 0 0 0 0)
                shouldBe
                    (intersectXRayWithFace (XRay3 0.5 0.0) face)
                    (Just $ XRayFaceIntersection 0 0 0 0)
                shouldBe
                    (intersectXRayWithFace (XRay3 1.0 0.0) face)
                    (Just $ XRayFaceIntersection 0 0 0 0)
                shouldBe
                    (intersectXRayWithFace (XRay3 0.0 0.5) face)
                    (Just $ XRayFaceIntersection 0 0 0 0)
                shouldBe
                    (intersectXRayWithFace (XRay3 0.5 0.5) face)
                    (Just $ XRayFaceIntersection 0 0 0 0)
                shouldBe
                    (intersectXRayWithFace (XRay3 1.0 0.5) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 0.0 1.0) face)
                    (Just $ XRayFaceIntersection 0 0 0 0)
                shouldBe
                    (intersectXRayWithFace (XRay3 0.5 1.0) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 1.0 1.0) face)
                    Nothing
            it "oblique triangle" $ do
                let face =
                        makeFace
                            [ Vec3 2 0 0
                            , Vec3 0 0 1
                            , Vec3 1 1 0
                            ]
                            ()
                shouldBe
                    (intersectXRayWithFace (XRay3 0.0 0.0) face)
                    (Just $ XRayFaceIntersection 2.0 2.0 (-1%2) (-5%6))
                shouldBe
                    (intersectXRayWithFace (XRay3 0.5 0.0) face)
                    (Just $ XRayFaceIntersection 1.5 1.5 (1%2) (-5%6))
                shouldBe
                    (intersectXRayWithFace (XRay3 1.0 0.0) face)
                    (Just $ XRayFaceIntersection 1.0 1.0 (1%2) (-5%6))
                shouldBe
                    (intersectXRayWithFace (XRay3 0.0 0.5) face)
                    (Just $ XRayFaceIntersection 1.0 1.0 (4%5) (-5%6))
                shouldBe
                    (intersectXRayWithFace (XRay3 0.5 0.5) face)
                    (Just $ XRayFaceIntersection 0.5 0.5 (5%6) (-5%6))
                shouldBe
                    (intersectXRayWithFace (XRay3 1.0 0.5) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 0.0 1.0) face)
                    (Just $ XRayFaceIntersection 0.0 0.0 (4%5) (-5%6))
                shouldBe
                    (intersectXRayWithFace (XRay3 0.5 1.0) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 1.0 1.0) face)
                    Nothing
                shouldBe
                    (intersectXRayWithFace (XRay3 0.25 0.25) face)
                    (Just $ XRayFaceIntersection 1.25 1.25 (5%6) (-5%6))
        describe "markFaceIntersectionWith" $ do
            let face =
                    makeFace
                        [ Vec3  0  0  0
                        , Vec3 10  0  0
                        , Vec3 10 10 20
                        , Vec3  0 10 20
                        ]
                        ()
            let inFacePS x y z =
                    toPlaneSpace (facePlaneSpace face) $ Vec3 x y z
            it "same face" $ do
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3 10  0  0
                            , Vec3  0  0  0
                            , Vec3  0 10 20
                            , Vec3 10 10 20
                            ]
                            ()
                    )
                    ( False
                    , face
                    )
            it "quad shares corner" $ do
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3 10  3  6
                            , Vec3 10 10 20
                            , Vec3  5 10 20
                            , Vec3  5  3  6
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS  5 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  3  6
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS 10  3  6)
                                (inFacePS  5  3  6)
                            , line2Between
                                (inFacePS  5  3  6)
                                (inFacePS  5 10 20)
                            ]
                        }
                    )
            it "quad shares side" $ do
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3  7  3  6
                            , Vec3  7 10 20
                            , Vec3  6 10 20
                            , Vec3  5 10 20
                            , Vec3  5  3  6
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS  5 10 20
                            , inFacePS  6 10 20
                            , inFacePS  7 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS  7  3  6)
                                (inFacePS  5  3  6)
                            , line2Between
                                (inFacePS  7 10 20)
                                (inFacePS  7  3  6)
                            , line2Between
                                (inFacePS  5  3  6)
                                (inFacePS  5 10 20)
                            ]
                        }
                    )
            it "quad intersects side" $ do
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3  7  3  6
                            , Vec3  7 15 30
                            , Vec3  5 15 30
                            , Vec3  5  3  6
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS  5 10 20
                            , inFacePS  7 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS  7  3  6)
                                (inFacePS  5  3  6)
                            , line2Between
                                (inFacePS  7 10 20)
                                (inFacePS  7  3  6)
                            , line2Between
                                (inFacePS  5  3  6)
                                (inFacePS  5 10 20)
                            ]
                        }
                    )
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3 12  3  6
                            , Vec3 12 15 30
                            , Vec3  5 15 30
                            , Vec3  5  3  6
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS  5 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  3  6
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS 10  3  6)
                                (inFacePS  5  3  6)
                            , line2Between
                                (inFacePS  5  3  6)
                                (inFacePS  5 10 20)
                            ]
                        }
                    )
            it "quad fully inside" $ do
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3  7  3  6
                            , Vec3  7  8 16
                            , Vec3  5  8 16
                            , Vec3  5  3  6
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS  7  3  6)
                                (inFacePS  5  3  6)
                            , line2Between
                                (inFacePS  7  8 16)
                                (inFacePS  7  3  6)
                            , line2Between
                                (inFacePS  5  8 16)
                                (inFacePS  7  8 16)
                            , line2Between
                                (inFacePS  5  3  6)
                                (inFacePS  5  8 16)
                            ]
                        }
                    )
            it "quad intersects face" $ do
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3  0 10  0
                            , Vec3 10 10  0
                            , Vec3 10  0 20
                            , Vec3  0  0 20
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  5 10
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            , inFacePS  0  5 10
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS  0  5 10)
                                (inFacePS 10  5 10)
                            ]
                        }
                    )
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3  2 10  0
                            , Vec3  8 10  0
                            , Vec3 10  0 20
                            , Vec3  0  0 20
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS  1  5 10)
                                (inFacePS  9  5 10)
                            ]
                        }
                    )
            it "quad touches face" $ do
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3  0  5 10
                            , Vec3 10  5 10
                            , Vec3 10  0 20
                            , Vec3  0  0 20
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  5 10
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            , inFacePS  0  5 10
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS  0  5 10)
                                (inFacePS 10  5 10)
                            ]
                        }
                    )
                shouldBe
                    (markFaceIntersectionWith face $
                        makeFace
                            [ Vec3  1  5 10
                            , Vec3  9  5 10
                            , Vec3  9  0 20
                            , Vec3  1  0 20
                            ]
                            ()
                    )
                    ( True
                    , face
                        { facePlanePoints =
                            [ inFacePS  0 10 20
                            , inFacePS 10 10 20
                            , inFacePS 10  0  0
                            , inFacePS  0  0  0
                            ]
                        , facePendingCuts =
                            [ line2Between
                                (inFacePS  1  5 10)
                                (inFacePS  9  5 10)
                            ]
                        }
                    )
