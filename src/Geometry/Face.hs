module Geometry.Face where

import Control.Exception
import Control.Monad
import Data.Interval
import Data.MinMaxWith
import Data.Monoid
import Data.NubList
import Data.Sequence (Seq (..))
import Geometry.BBox
import Geometry.Class
import Geometry.PlaneSpace
import Geometry.Vec2
import Geometry.Vec3
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.CSeq as CSeq
import qualified Data.Sequence as Seq

data Face a = Face
    { facePlaneSpace :: PlaneSpace
    , facePlanePoints :: Seq Vec2
    , facePlaneRegion :: Seq HalfPlane
    , faceCenter :: Vec3
    , faceBBox :: BBox
    , faceData :: a
    }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

Aeson.deriveJSON Aeson.defaultOptions ''Face

instance (Show a) => Show (Face a) where
    showsPrec d face =
        showParen (d > 10) $
            showString "makeFace " .
            showsPrec 11 (facePoints face) .
            showString " " .
            showsPrec 11 (faceData face)

instance HasBBox (Face a) where
    bboxOf = faceBBox

instance (ToWolfMod a) => ToWolf (Face a) where
    toWolf face =
        wolfApp
            "face"
            [ wolfList $ toWolfMod (faceData face)
            , toWolf $ facePoints face
            , toWolf $ facePlanePoints face
            ]

facePoints :: Face a -> Seq Vec3
facePoints face =
    fmap
        (fromPlaneSpace (facePlaneSpace face))
        (facePlanePoints face)

faceHalfSpace :: Face a -> HalfSpace
faceHalfSpace = planeSpaceHalfSpace . facePlaneSpace

faceNormal :: Face a -> IVec3
faceNormal = halfSpaceNormal . faceHalfSpace

data FaceSide = FaceBack | FaceFront
  deriving (Show, Eq, Ord)

findPolygonHalfSpace :: Seq Vec3 -> HalfSpace
findPolygonHalfSpace vs =
    case vs of
        v1 :<| v2 :<| rest -> go v1 (v2 -. v1) rest
        _ -> bad
  where
    go _ _ Empty = bad
    go o a (q :<| qs) = do
        let b = q -. o
        let n = cross3 a b
        if n == Vec3 0 0 0
            then go o a qs
            else halfSpaceFromNormalAndOrigin n o
    bad = error "degenerate face"

findPolygonCenter :: Seq Vec2 -> Vec2
findPolygonCenter (u0 :<| us)
    | den /= 0 =
        recip (3 * den) *. vecNum +. ((1/3) *. u0)
  where
    (vecNum, den) =
        CSeq.foldrPairsInner
            (\a b (pacc, wacc) ->
                let w = det2 (a -. u0) (b -. u0) in
                ( pacc +. w *. (a +. b)
                , wacc + w
                )
            )
            (Vec2 0 0, 0)
            us
findPolygonCenter _ = error "degenerate face"

checkFaceIsConvex :: Face a -> Face a
checkFaceIsConvex face = do
    assert (all isGoodEdge (facePlaneRegion face)) face
  where
    isGoodEdge edge =
        all (\p -> homdot2 edge p >= 0) (facePlanePoints face)

makeFace :: Seq Vec3 -> a -> Face a
makeFace vs d = do
    let hs1 = findPolygonHalfSpace vs
    let (hsc, vsc) =
            if isCanonicalIVec3 (halfSpaceNormal hs1)
                then (hs1, vs)
                else (inverseHalfSpace hs1, Seq.reverse vs)
    let ps = planeSpaceFromHalfSpace hsc
    let us = fmap (toPlaneSpace ps) vsc
    (makeFaceInPlane ps us d)
        { faceBBox = foldableBBox $ fmap pointBBox vs
        }
  where
    isCanonicalIVec3 n =
        n >= IVec3 0 0 0

makeFaceInPlane :: PlaneSpace -> Seq Vec2 -> a -> Face a
makeFaceInPlane hs ps d =
    checkFaceIsConvex $
    Face
        { facePlaneSpace = hs
        , facePlanePoints = ps
        , facePlaneRegion = CSeq.mapPairsLoop leftHalfPlane ps
        , faceCenter = fromPlaneSpace hs (findPolygonCenter ps)
        , faceBBox = foldableBBox $ fmap (pointBBox . fromPlaneSpace hs) ps
        , faceData = d
        }

simplifyContour :: Seq Vec2 -> Seq Vec2
simplifyContour =
    CSeq.foldMapTriplesLoop $ \a b c ->
        if det2 (b -. a) (c -. b) == 0
            then Empty
            else Seq.singleton b

tcon :: Seq Vec2
tcon = Seq.fromList
    [ Vec2  0  0
    , Vec2 10  0
    , Vec2 10 10
    , Vec2  0 10
    ]

tconhps :: Seq HalfPlane
tconhps = CSeq.mapPairsLoop leftHalfPlane tcon

tcon2 :: Seq Vec2
tcon2 = Seq.fromList
    [ Vec2 10  2
    , Vec2 20  2
    , Vec2 20  8
    , Vec2 10  8
    ]

splitContourByNormOffset ::
    Vec2 ->
    Rational ->
    Seq Vec2 ->
    (Seq Vec2, Seq Vec2)
splitContourByNormOffset norm offset ps =
    case CSeq.foldMapPairsLoop onEdge $ fmap attachSignedDistance ps of
        (nc, pc) ->
            (simplifyContour nc, simplifyContour pc)
  where
    attachSignedDistance a = (dot norm a + offset, a)
    onEdge (hda, a) (hdb, b) = do
        let apseq =
                if hda >= 0
                    then Seq.singleton a
                    else Empty
        let anseq =
                if hda <= 0
                    then Seq.singleton a
                    else Empty
        let bseq =
                if (hda > 0 && hdb < 0) || (hda < 0 && hdb > 0)
                    then Seq.singleton $
                        (hda / (hda - hdb)) *. (b -. a) +. a
                    else Empty
        (anseq <> bseq, apseq <> bseq)

splitContourByHalfPlane ::
    HalfPlane ->
    Seq Vec2 ->
    (Seq Vec2, Seq Vec2)
splitContourByHalfPlane (HalfPlane inorm ioffset) ps =
    splitContourByNormOffset
        (vec2FromInteger inorm)
        (fromInteger ioffset)
        ps

splitContourByOther ::
    Seq HalfPlane ->
    Seq Vec2 ->
    Seq Vec2 ->
    ([Seq Vec2], Seq Vec2)
splitContourByOther hps ps0 split0 =
    CSeq.foldrPairsLoop
        processEdge
        ([], ps0)
        split0
  where
    processEdge a b (buf, ps) = do
        let mbABRange =
                clipLineRange
                    (line2Between a b)
                    (Interval (Interior 0) (Interior 1))
                    hps
        let abNormal = cross2 (b -. a)
        let abOffset = - dot a abNormal
        case mbABRange of
            Nothing -> do
                (buf, ps)
            Just _ -> do
                let (outside, inside) =
                        splitContourByNormOffset
                            abNormal
                            abOffset
                            ps
                let buf2 =
                        if Seq.null outside
                            then buf
                            else outside : buf
                (buf2, inside)

splitContourByHalfPlaneSeq ::
    Seq Vec2 ->
    Seq HalfPlane ->
    ([Seq Vec2], Seq Vec2)
splitContourByHalfPlaneSeq ps0 hps0 =
    foldr processHP ([], ps0) hps0
  where
    processHP hp (buf, ps) =
        case splitContourByHalfPlane hp ps of
            (outside, inside) ->
                ( [outside | not $ Seq.null outside] ++ buf
                , inside
                )

splitFaceWithHalfPlane ::
    HalfPlane -> Face a -> ([Face a], [Face a])
splitFaceWithHalfPlane hp face =
    case splitContourByHalfPlane hp (facePlanePoints face) of
        (psa, psb) ->
            (   [ makeFaceInPlane
                    (facePlaneSpace face)
                    psa
                    (faceData face)
                | not $ Seq.null psa
                ]
            ,   [ makeFaceInPlane
                    (facePlaneSpace face)
                    psb
                    (faceData face)
                | not $ Seq.null psb
                ]
            )

splitFace ::
    Face a ->
    Face b ->
    ([Face a], Maybe (Face a))
splitFace fa fb =
    case halfSpaceRelation (faceHalfSpace fa) (faceHalfSpace fb) of
        HalfSpaceCoplanar _ -> do
            let (aPieces, aLeftover) =
                    splitContourByOther
                        (facePlaneRegion fa)
                        (facePlanePoints fa)
                        (facePlanePoints fb)
            if Seq.null aLeftover
                then ([fa], Nothing)
                else do
                    let leftoverHPs =
                            CSeq.mapPairsLoop leftHalfPlane aLeftover
                    let (_bPieces, centerPiece) =
                            splitContourByHalfPlaneSeq
                                (facePlanePoints fb)
                                leftoverHPs
                    if Seq.null centerPiece
                        then
                            assert (null aPieces) $
                            ([fa], Nothing)
                        else
                            assert
                                (   CSeq.canonicalRotation aLeftover ==
                                    CSeq.canonicalRotation centerPiece
                                ) $
                            ( fmap
                                (\ps -> makeFaceInPlane
                                    (facePlaneSpace fa)
                                    ps
                                    (faceData fa))
                                aPieces
                            , Just $
                                makeFaceInPlane
                                    (facePlaneSpace fa)
                                    centerPiece
                                    (faceData fa)
                            )
        HalfSpaceParallel ->
            ([fa], Nothing)
        HalfSpaceIntersect line3 -> do
            let mbCommonRange = do
                    rangeA <-
                        clipLineRange
                            (toPlaneSpaceLine (facePlaneSpace fa) line3)
                            (Interval NegInfinity PosInfinity)
                            (facePlaneRegion fa)
                    clipLineRange
                        (toPlaneSpaceLine (facePlaneSpace fb) line3)
                        rangeA
                        (facePlaneRegion fb)
            case mbCommonRange of
                Nothing ->
                    ([fa], Nothing)
                Just _ -> do
                    let (mfa1, mfa2) =
                            splitFaceWithHalfPlane
                                (toPlaneSpaceHalfSpace
                                    (facePlaneSpace fa)
                                    (faceHalfSpace fb)
                                )
                                fa
                    (mfa1 ++ mfa2, Nothing)

clipLineRange ::
    Line2 ->
    Interval (Extended Rational) ->
    Seq HalfPlane ->
    Maybe (Interval (Extended Rational))
clipLineRange line initialInterval edges = do
    let result@(Interval mka mkb) =
            foldr
                (\e i ->
                    intersect (line2ClipWithHalfPlane line e) i
                )
                initialInterval edges
    guard (mka < mkb)
    Just result

{-  Face's plane space is canonicalized, such that the right (positive X) side
    is always the front one. -}
data XRayFaceIntersection = XRayFaceIntersection
    { xrayFaceIntersectionRightXPos :: {-# UNPACK #-} !Rational
    , xrayFaceIntersectionLeftXPos :: {-# UNPACK #-} !Rational
    , xrayFaceIntersectionTangentConeAngle :: {-# UNPACK #-} !Rational
    , xrayFaceIntersectionPlaneAngle :: {-# UNPACK #-} !Rational
    }
  deriving (Show, Eq, Ord)

intersectXRayWithFace ::
    XRay3 ->
    Face a ->
    Maybe XRayFaceIntersection
intersectXRayWithFace xray face = do
    guard (bboxContainsXRay3 xray (faceBBox face))
    case intersectXRayWithPlaneSpace xray (facePlaneSpace face) of
        XRayPlaneSpaceIntersectionPoint _ hitX point ->
            intersectXRayWithFaceAtPoint face hitX point
        XRayPlaneSpaceIntersectionInside uray -> do
            intersectXRayWithFaceAtLine face uray
        _ ->
            Nothing

intersectXRayWithFaceAtPoint ::
    Face a ->
    Rational ->
    Vec2 ->
    Maybe XRayFaceIntersection
intersectXRayWithFaceAtPoint face hitX point = do
    NubList edgeNorms <-
        getAp $ foldMap (Ap . onEdgeHalfPlane) (facePlaneRegion face)
    let nascent = 1 - squareXCos3i (halfSpaceNormal (faceHalfSpace face))
    tangentConeAngle <-
        case edgeNorms of
            _ | nascent == 0 -> do
                Just 0
            [] -> do
                Just nascent
            [edgeNormA] -> do
                let IVec2 enxa _ = edgeNormA
                let edgeAscent =
                        squareXCosFromPlaneSpace
                            (facePlaneSpace face)
                            (cross2i edgeNormA)
                if enxa >= 0
                    then Just nascent
                    else Just edgeAscent
            [edgeNormA, edgeNormB] -> do
                let IVec2 enxa _ = edgeNormA
                let IVec2 enxb _ = edgeNormB
                let edgeA = makeVecPositiveX (cross2i edgeNormA)
                let edgeB = makeVecPositiveX (cross2i edgeNormB)
                let ascentA =
                        squareXCosFromPlaneSpace (facePlaneSpace face) edgeA
                let ascentB =
                        squareXCosFromPlaneSpace (facePlaneSpace face) edgeB
                let (edgeP, edgeNormP, ascentP, edgeQ, edgeNormQ, ascentQ) =
                        if ascentA <= ascentB
                            then
                                ( edgeA, edgeNormA, ascentA
                                , edgeB, edgeNormB, ascentB
                                )
                            else
                                ( edgeB, edgeNormB, ascentB
                                , edgeA, edgeNormA, ascentA
                                )
                if
                    | enxa >= 0 && enxb >= 0 ->
                        Just nascent
                    | dot edgeQ edgeNormP >= 0 ->
                        Just ascentQ
                    | dot edgeP edgeNormQ >= 0 ->
                        Just ascentP
                    | otherwise ->
                        Just (-ascentP)
            _ -> do
                Nothing
    Just $ XRayFaceIntersection
        { xrayFaceIntersectionRightXPos = hitX
        , xrayFaceIntersectionLeftXPos = hitX
        , xrayFaceIntersectionTangentConeAngle = tangentConeAngle
        , xrayFaceIntersectionPlaneAngle = -nascent
        }
  where
    onEdgeHalfPlane e = do
        let hd = homdot2 e point
        if
            | hd < 0 ->
                Nothing
            | hd > 0 ->
                Just (NubList [])
            | otherwise ->
                Just (NubList [halfPlaneNormal e])
    makeVecPositiveX :: IVec2 -> IVec2
    makeVecPositiveX v@(IVec2 x _)
        | x >= 0 = v
        | otherwise = negateVec v

intersectXRayWithFaceAtLine ::
    Face a ->
    XRay2 ->
    Maybe XRayFaceIntersection
intersectXRayWithFaceAtLine face uray = do
    (     Just (MaxWith leftX (NubList leftEdgeNorms))
        , Just (MinWith rightX (NubList rightEdgeNorms))
        , NubList tangentEdgeNorms
        ) <-
            getAp $ foldMap (Ap . onEdgeHalfPlane) (facePlaneRegion face)
    guard (leftX <= rightX)
    let edgeNorms =
            if leftX == rightX
                then leftEdgeNorms <> rightEdgeNorms <> tangentEdgeNorms
                else rightEdgeNorms <> tangentEdgeNorms
    tangentConeAngle <-
        case edgeNorms of
            [edgeNormA] -> do
                let edgeAscent =
                        squareXCosFromPlaneSpace
                            (facePlaneSpace face)
                            (cross2i edgeNormA)
                Just edgeAscent
            [edgeNormA, edgeNormB] -> do
                let edgeA = makeVecPositiveX (cross2i edgeNormA)
                let edgeB = makeVecPositiveX (cross2i edgeNormB)
                let ascentA =
                        squareXCosFromPlaneSpace (facePlaneSpace face) edgeA
                let ascentB =
                        squareXCosFromPlaneSpace (facePlaneSpace face) edgeB
                let (edgeP, edgeNormP, ascentP, edgeQ, edgeNormQ, ascentQ) =
                        if ascentA <= ascentB
                            then
                                ( edgeA, edgeNormA, ascentA
                                , edgeB, edgeNormB, ascentB
                                )
                            else
                                ( edgeB, edgeNormB, ascentB
                                , edgeA, edgeNormA, ascentA
                                )
                if
                    | dot edgeQ edgeNormP >= 0 ->
                        Just ascentQ
                    | dot edgeP edgeNormQ >= 0 ->
                        Just ascentP
                    | otherwise ->
                        Just (-ascentP)
            _ -> do
                Nothing
    Just $ XRayFaceIntersection
        { xrayFaceIntersectionRightXPos = rightX
        , xrayFaceIntersectionLeftXPos = leftX
        , xrayFaceIntersectionTangentConeAngle = tangentConeAngle
        , xrayFaceIntersectionPlaneAngle = -1
        }
  where
    onEdgeHalfPlane e = do
        case intersectXRayWithHalfPlane uray e of
            XRayHalfPlaneIntersectionPoint XRay2Right px ->
                Just
                    ( Just (MaxWith px (NubList [halfPlaneNormal e]))
                    , Nothing
                    , NubList []
                    )
            XRayHalfPlaneIntersectionPoint XRay2Left px ->
                Just
                    ( Nothing
                    , Just (MinWith px (NubList [halfPlaneNormal e]))
                    , NubList []
                    )
            XRayHalfPlaneIntersectionInside ->
                Just
                    ( Nothing
                    , Nothing
                    , NubList [halfPlaneNormal e]
                    )
            XRayHalfPlaneIntersectionFront ->
                Just
                    ( Nothing
                    , Nothing
                    , NubList []
                    )
            XRayHalfPlaneIntersectionBack ->
                Nothing
    makeVecPositiveX :: IVec2 -> IVec2
    makeVecPositiveX v@(IVec2 x _)
        | x >= 0 = v
        | otherwise = negateVec v
