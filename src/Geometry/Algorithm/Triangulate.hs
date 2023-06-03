module Geometry.Algorithm.Triangulate where

import Control.Monad
import Control.Monad.ST
import Control.Exception
import Data.CList
import Data.Foldable
import Data.Interval
import Data.Ratio
import Data.Semigroup
import Data.Sequence (Seq (..))
import Geometry.AngleRat
import Geometry.Brush.Type
import Geometry.Class
import Geometry.Face
import Geometry.PlaneSpace
import Geometry.Vec2
import Math.NumberTheory.Roots
import qualified Data.CSeq as CSeq
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap
import qualified Data.IntervalMap as IntervalMap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MutVector




makeBrushPlaneMaps ::
    (Ord a) =>
    Brush a b ->
    Map.Map (PlaneSpace, a) PlaneMap
makeBrushPlaneMaps brush =
    MonoidMap.unwrapMonoidMap $
        foldMap
            (\face -> do
                MonoidMap.singleton
                    (facePlaneSpace face, faceData face)
                    (contourPlaneMap $ facePlanePoints face)
            )
            (brushFaces brush)

triangulatePlaneMap ::
    PlaneMap ->
    Seq (Vec2, Vec2, Vec2)
triangulatePlaneMap pm =
    foldMap
        (triangulatePolygon . flattenArea)
        (splitToAreas $ planeMapToSegments pm)

newtype PlaneMap = PlaneMap
    { unwrapPlaneMap ::
        MonoidMap.MonoidMap
            Axis2
            (IntervalMap.IntervalMap Rational (Sum Int))
    }
    deriving (Show)
    deriving newtype (Semigroup, Monoid)

planeMapToSegments ::
    PlaneMap ->
    Set.Set (Vec2, Vec2)
planeMapToSegments (PlaneMap axismap) =
    Map.foldMapWithKey
        (\axis intmap ->
            IntervalMap.foldMapIntervals
                (\(Interval r1 r2) (Sum i) ->
                    case i of
                        1 ->
                            Set.singleton
                                (axis2Point axis r1, axis2Point axis r2)
                        -1 ->
                            Set.singleton
                                (axis2Point axis r2, axis2Point axis r1)
                        0 ->
                            Set.empty
                        _ ->
                            error "invalid PlaneMap"
                )
                intmap
        )
        (MonoidMap.unwrapMonoidMap axismap)

instance ToWolf PlaneMap where
    toWolf pm =
        wolfList $
            foldMap
                (\(a, b) -> [wolfApp "Polygon" [wolfList (polyPts a b)]])
                (planeMapToSegments pm)
      where
        polyPts a b =
            let h = b -. a ; v = cross2 h in
            [ toWolf a
            , toWolf b
            , toWolf $ b -. 0.05 *. h +. 0.03 *. v
            , toWolf $ a +. 0.05 *. h +. 0.03 *. v
            ]

edgePlaneMap ::
    Vec2 ->
    Vec2 ->
    PlaneMap
edgePlaneMap a b = do
    let (abInverted, axis) = makeAxis2Between a b
    let ar = axis2Dot axis a
    let br = axis2Dot axis b
    PlaneMap $
        MonoidMap.singleton axis $
            if abInverted
                then IntervalMap.singleton (Interval br ar) (Sum (-1))
                else IntervalMap.singleton (Interval ar br) (Sum 1)

contourPlaneMap ::
    Seq Vec2 ->
    PlaneMap
contourPlaneMap ps =
    CSeq.foldMapPairsLoop edgePlaneMap ps

removeRedundantPoints ::
    Seq Vec2 ->
    Seq Vec2
removeRedundantPoints (v0 :<| v1 :<| vs) =
    go (v0 -. v1) v1 (vs :|> v0 :|> v1)
  where
    go _ _ Empty = Empty
    go toPrev b (c :<| rest) = do
        let toNext = c -. b
        if det2 toPrev toNext == 0
            then go toPrev c rest
            else b :<| go (negateVec toNext) c rest
removeRedundantPoints _ =
    error "degenerate contour"

peelContour ::
    Set.Set (Vec2, Vec2) ->
    Maybe (Seq Vec2, Rational, Set.Set (Vec2, Vec2))
peelContour pm0 =
    case Set.minView pm0 of
        Nothing -> Nothing
        Just ((a, b), pm1) -> Just (go (Seq.singleton a) a a b 0 pm1)
  where
    go buf firstPoint lastPoint currentPoint signedArea pm
        | currentPoint == firstPoint =
            (removeRedundantPoints buf, signedArea, pm)
        | otherwise =
            case findNextPoint lastPoint currentPoint pm of
                (nextPoint, pm2) -> do
                    let newArea =
                            det2
                                (currentPoint -. firstPoint)
                                (nextPoint -. firstPoint)
                    go
                        (buf :|> currentPoint)
                        firstPoint
                        currentPoint
                        nextPoint
                        (signedArea + newArea)
                        pm2

findNextPoint ::
    Vec2 ->
    Vec2 ->
    Set.Set (Vec2, Vec2) ->
    (Vec2, Set.Set (Vec2, Vec2))
findNextPoint a b pm0 = do
    let !(pmlt, pmeq, pmgt) =
            setSplitAntitone
                (\(u, _) -> compare u b)
                pm0
    let (aList, candList) =
            List.partition (== a) $ map snd $ Set.toList pmeq
    case tryTakeMinimumOn (\c -> angleRat2 (c -. b) (a -. b)) candList of
        Nothing -> error "invalid PlaneMap"
        Just (cn, otherPts) -> do
            let pmeq2 = Set.fromList (map (\v -> (b, v)) (aList <> otherPts))
            (cn, pmlt `Set.union` pmeq2 `Set.union` pmgt)

setSplitAntitone ::
    (Ord a) =>
    (a -> Ordering) ->
    Set.Set a ->
    (Set.Set a, Set.Set a, Set.Set a)
setSplitAntitone fn set = do
    let gt = Set.dropWhileAntitone (\x -> fn x <= EQ) set
    let le = Set.takeWhileAntitone (\x -> fn x <= EQ) set
    let lt = Set.takeWhileAntitone (\x -> fn x == LT) le
    let eq = Set.dropWhileAntitone (\x -> fn x == LT) le
    assert (Set.unions [lt,eq,gt] == set) $ (lt, eq, gt)

data Area = Area (Seq Vec2) (Seq (Seq Vec2))
    deriving (Show)

instance ToWolf Area where
    toWolf (Area contour holes) =
        wolfList $
            foldMap
                (\(a, b) -> [wolfApp "Polygon" [wolfList (polyPts a b)]])
                (toSegs contour <> foldMap toSegs holes)
      where
        toSegs = CSeq.mapPairsLoop (,)
        polyPts a b =
            let h = b -. a ; v = cross2 h in
            [ toWolf a
            , toWolf b
            , toWolf $ b -. 0.05 *. h +. 0.03 *. v
            , toWolf $ a +. 0.05 *. h +. 0.03 *. v
            ]

splitToAreas ::
    Set.Set (Vec2, Vec2) ->
    [Area]
splitToAreas = go []
  where
    go current pm1 =
        case peelContour pm1 of
            Nothing -> current
            Just (c, signedArea, pm2)
                | signedArea > 0 ->
                    go (Area c Empty : current) pm2
                | signedArea < 0 ->
                    go (insertHole c current) pm2
                | otherwise ->
                    error "degenerate contour"
    insertHole _ [] =
        error "hole outside of polygons"
    insertHole holeContour (area@(Area aContour aHoles) : rest) = do
        let !pt =
                case holeContour of
                    ha :<| _ -> ha
                    _ -> error "degenerate hole"
        case windingCharacteristic pt aContour of
            WindingComplexZero ->
                (area :) $! insertHole holeContour rest
            WindingComplexTwoPi ->
                Area aContour (holeContour :<| aHoles) : rest
            _ ->
                error "invalid area"

data WindingComplex
    = WindingComplex
        {-# UNPACK #-} !Int
        !Integer
        !Integer
    | WindingComplexUndefined
    deriving (Show, Eq)

pattern WindingComplexZero :: WindingComplex
pattern WindingComplexZero = WindingComplex 0 1 0

pattern WindingComplexTwoPi :: WindingComplex
pattern WindingComplexTwoPi = WindingComplex 4 1 0

instance Semigroup WindingComplex where
    WindingComplex wa xa ya <> WindingComplex wb xb yb =
        assert (xa > 0 && ya >= 0 && xb > 0 && yb >= 0) $ do
            let xt = xa * xb - ya * yb
            let yt = xa * yb + xb * ya
            let cden = xt `gcd` yt
            let xn = xt `quot` cden
            let yn = yt `quot` cden
            if xn > 0
                then WindingComplex (wa + wb) xn yn
                else WindingComplex (wa + wb + 1) yn (- xn)
    _ <> _ = WindingComplexUndefined

instance Monoid WindingComplex where
    mempty = WindingComplexZero

windingComplexFromXY ::
    Rational ->
    Rational ->
    WindingComplex
windingComplexFromXY x y
    | x > 0 && y >= 0 =
        WindingComplex 0 xn yn
    | x >= 0 && y < 0 =
        WindingComplex (-1) (-yn) xn
    | x <= 0 && y > 0 =
        WindingComplex 1 yn (-xn)
    | x < 0 && y < 0 =
        WindingComplex (-2) (-xn) (-yn)
    | otherwise =
        WindingComplexUndefined
  where
    xi = numerator x * denominator y
    yi = denominator x * numerator y
    cden = xi `gcd` yi
    xn = xi `quot` cden
    yn = yi `quot` cden

windingCharacteristic ::
    Vec2 ->
    Seq Vec2 ->
    WindingComplex
windingCharacteristic center ps =
    CSeq.foldMapPairsLoop wcBetween $ fmap (-. center) ps
  where
    wcBetween (Vec2 xa ya) (Vec2 xb yb) =
        windingComplexFromXY
            (xa * xb + ya * yb)
            (xa * yb - xb * ya)

distanceCost ::
    Vec2 ->
    Integer
distanceCost v =
    case integerSquareRoot (truncate (256 * dotSqr v)) of
        0 -> 1
        r -> r

flattenArea ::
    Area ->
    Seq Vec2
flattenArea area@(Area outline holes) =
    case holes of
        Empty -> outline
        _ ->
            case minimumAllowedOuterCut area of
                Nothing ->
                    error "cannot cut area with holes"
                Just (Min (Arg _ (newOutline, leftoverHoles))) ->
                    flattenArea $ Area newOutline leftoverHoles

minimumAllowedOuterCut ::
    Area ->
    Maybe (Min (Arg Integer (Seq Vec2, Seq (Seq Vec2))))
minimumAllowedOuterCut (Area outline holes) =
    flip CSeq.foldMapExclusions holes
        $ \holesPrefix thisHole holesSuffix ->
            minimumAllowedCutBetween
                outline
                thisHole
                (holesPrefix <> holesSuffix)

minimumAllowedCutBetween ::
    Seq Vec2 ->
    Seq Vec2 ->
    Seq (Seq Vec2) ->
    Maybe (Min (Arg Integer (Seq Vec2, Seq (Seq Vec2))))
minimumAllowedCutBetween shapeA shapeB otherShapes =
    flip foldMapOutlineCorners shapeA $ \aPoint aDir aAngle aStrip ->
    flip foldMapOutlineCorners shapeB $ \bPoint bDir bAngle bStrip -> do
        let segmentIsNull = aPoint == bPoint
        let thisSegment@(Line2 _ segmentDelta) = line2Between aPoint bPoint
        let segmentIsExterior =
                (angleRat2 aDir segmentDelta > aAngle) ||
                (angleRat2 bDir (negateVec segmentDelta) > bAngle)
        let thisSegmentBlockedBy a b =
                Any $ cutBlockedBy thisSegment (line2Between a b)
        let Any segmentIsBlocked =
                CSeq.foldMapPairsInner
                    thisSegmentBlockedBy
                    aStrip
                <>
                CSeq.foldMapPairsInner
                    thisSegmentBlockedBy
                    bStrip
                <>
                foldMap
                    (\holePoints ->
                        CSeq.foldMapPairsLoop
                            thisSegmentBlockedBy
                            holePoints
                    )
                    otherShapes
        if segmentIsNull || segmentIsExterior || segmentIsBlocked
            then
                Nothing
            else do
                let newOutline =
                        Seq.singleton aPoint <>
                        aStrip <>
                        Seq.singleton aPoint <>
                        Seq.singleton bPoint <>
                        bStrip <>
                        Seq.singleton bPoint
                Just $ Min $
                    Arg
                        (distanceCost segmentDelta)
                        (newOutline, otherShapes)

triangulatePolygon ::
    Seq Vec2 ->
    Seq (Vec2, Vec2, Vec2)
triangulatePolygon pointSeq = do
    runST $ do
        buffer <- MutVector.new (cornerCount * cornerCount)
        let readBuffer ai bi = do
                MutVector.read buffer (ai * cornerCount + bi)
        let writeBuffer ai bi x = do
                MutVector.write buffer (ai * cornerCount + bi) x
        forM_ [0 .. cornerCount - 2] $ \i -> do
            case cornerVec Vector.! i of
                (_, dir, _) -> do
                    writeBuffer i (i + 1) $
                        Just $ Min $ Arg (distanceCost dir) Seq.empty
        forM_ [2 .. cornerCount - 1] $ \delta -> do
            forM_ [0 .. cornerCount - delta - 1] $ \i -> do
                let j = i + delta
                case cutBetween i j of
                    Nothing -> do
                        writeBuffer i j Nothing
                    Just cutCost -> do
                        variants <- forM [i + 1 .. j - 1] $ \v -> do
                            mbLeftState <- readBuffer i v
                            mbRightState <- readBuffer v j
                            pure $ do
                                Min (Arg leftCost leftTris) <- mbLeftState
                                Min (Arg rightCost rightTris) <- mbRightState
                                Just $ Min $ Arg
                                    (leftCost + rightCost + cutCost)
                                    (   leftTris <>
                                        rightTris <>
                                        Seq.singleton (i, v, j)
                                    )
                        writeBuffer i j (fold variants)
        mbResult <- readBuffer 0 (cornerCount - 1)
        case mbResult of
            Nothing -> error "cannot triangulate"
            Just (Min (Arg _ tris)) -> do
                pure $
                    fmap
                        (\(i, j, k) ->
                            (cornerPointAt i, cornerPointAt j, cornerPointAt k)
                        )
                        tris
  where
    cutBetween ai bi =
        cutMap Vector.! (ai * cornerCount + bi)
    cutMap =
        Vector.create $ do
            mv <- MutVector.new (cornerCount * cornerCount)
            forM_ [0 .. cornerCount - 1] $ \bi -> do
                let (bPoint, bDir, bAngle) = cornerVec Vector.! bi
                forM_ [0 .. bi] $ \ai -> do
                    let (aPoint, aDir, aAngle) = cornerVec Vector.! ai
                    let thisSegment@(Line2 _ segmentDelta) =
                            line2Between aPoint bPoint
                    let segmentIsNull = aPoint == bPoint
                    let aRayAngle =
                            angleRat2 aDir segmentDelta
                    let bRayAngle =
                            angleRat2 bDir (negateVec segmentDelta)
                    let segmentIsExterior =
                            (aRayAngle == AngleRatPosX) ||
                            (aRayAngle >= aAngle) ||
                            (bRayAngle == AngleRatPosX) ||
                            (bRayAngle >= bAngle)
                    let abStrip =
                            Seq.drop (ai + 1) $ Seq.take bi pointSeq
                    let baStrip =
                            Seq.drop (bi + 1) pointSeq <>
                            Seq.take ai pointSeq
                    let thisSegmentBlockedBy a b =
                            Any $ cutBlockedBy
                                thisSegment
                                (line2Between a b)
                    let Any segmentIsBlocked =
                            CSeq.foldMapPairsInner
                                thisSegmentBlockedBy
                                abStrip
                            <>
                            CSeq.foldMapPairsInner
                                thisSegmentBlockedBy
                                baStrip
                    let segmentRejected =
                            segmentIsNull ||
                            segmentIsExterior ||
                            segmentIsBlocked
                    let result
                            | segmentRejected =
                                Nothing
                            | otherwise =
                                Just $ distanceCost segmentDelta
                    MutVector.write mv (ai * cornerCount + bi) result
            MutVector.write mv (0 * cornerCount + cornerCount - 1) $
                Just 0
            pure mv
    cornerVec = Vector.fromListN cornerCount (toList cornerSeq)
    cornerCount = Seq.length cornerSeq
    cornerSeq =
        flip CSeq.foldMapTriplesLoop pointSeq $ \prev x next -> do
            let toPrev = prev -. x
            let toNext = next -. x
            Seq.singleton (x, toNext, angleRat2 toNext toPrev)
    cornerPointAt i =
        case cornerVec Vector.! i of
            (pt, _, _) -> pt

cutBlockedBy ::
    Line2 ->
    Line2 ->
    Bool
cutBlockedBy (Line2 o1 d1) (Line2 o2 d2) = do
    let oo = o2 -. o1
    let den = det2 d1 d2
    if den == 0
        then do
            if det2 oo d1 == 0
                then do
                    {- on the same line -}
                    let p1 = dot o1 d1
                    let q1 = p1 + dot d1 d1
                    let wp2 = dot o2 d1
                    let wq2 = wp2 + dot d1 d2
                    let (p2, q2) =
                            if wp2 <= wq2
                                then (wp2, wq2)
                                else (wq2, wp2)
                    let maxp = max p1 p2
                    let minq = min q1 q2
                    maxp < minq
                else do
                    {- parallel -}
                    False
        else do
            {- intersect at point -}
            let k1 = det2 oo d2 / den
            let k2 = det2 oo d1 / den
            (0 < k1 && k1 < 1 && 0 <= k2 && k2 <= 1)

foldMapOutlineCorners ::
    (Monoid m) =>
    (Vec2 -> Vec2 -> AngleRat -> Seq Vec2 -> m) ->
    Seq Vec2 ->
    m
foldMapOutlineCorners tom vs =
    CSeq.foldMapExclusions folder vs
  where
    folder prefix x suffix = do
        let strip = suffix <> prefix
        case (strip, strip) of
            (next :<| _, _ :|> prev) -> do
                let toNext = next -. x
                let toPrev = prev -. x
                tom x toNext (angleRat2 toNext toPrev) strip
            _ ->
                error "degenerate contour"
