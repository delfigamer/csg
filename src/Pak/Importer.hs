module Pak.Importer where

import Common.Types
import Data.Bits
import Data.Foldable
import Data.Int
import Data.Interval
import Data.Semigroup
import Data.Ratio
import Data.Sequence (Seq (..))
import Geometry.BBox
import Geometry.Brush
import Geometry.Class
import Geometry.Face
import Geometry.Mat3
import Geometry.Vec3
import qualified Data.CSeq as CSeq
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Geometry.RTree as RTree
import qualified Pak.Polys as P

tJunctionThreshold :: Rational
tJunctionThreshold = 1

toVec3 :: P.Vector -> Vec3
toVec3 (P.Vector x y z) =
    Vec3 (- toRational x) (toRational y) (toRational z)

snapVec3 :: Vec3 -> Vec3
snapVec3 (Vec3 x y z) =
    Vec3 (snapCoord x) (snapCoord y) (snapCoord z)
  where
    snapCoord r = round r % 1
--     snap16 r = round (r * 16) % 16

shearMat ::
    P.ShearAxis ->
    Float ->
    Mat3
shearMat axis df =
    case axis of
        P.SANone -> Mat3Identity
        P.SAXY -> Mat3   1 0 0   d 1 0   0 0 1
        P.SAXZ -> Mat3   1 0 0   0 1 0   d 0 1
        P.SAYX -> Mat3   1 d 0   0 1 0   0 0 1
        P.SAYZ -> Mat3   1 0 0   0 1 0   0 d 1
        P.SAZX -> Mat3   1 0 d   0 1 0   0 0 1
        P.SAZY -> Mat3   1 0 0   0 1 d   0 0 1
  where
    d = toRational df

axisScaleMat ::
    Float ->
    Float ->
    Float ->
    Mat3
axisScaleMat sxf syf szf =
    Mat3   sx 0 0   0 sy 0   0 0 sz
  where
    sx = toRational sxf
    sy = toRational syf
    sz = toRational szf

scaleMat ::
    P.Scale ->
    Mat3
scaleMat (P.Scale sxf syf szf sdf sa) =
    shearMat sa sdf *!
    axisScaleMat sxf syf szf

rotation16iCosSin ::
    Int32 ->
    (Rational, Rational)
rotation16iCosSin i0 =
    normalizeResult $
    innerStep      0    0xffffffec42c      0x6487ed4e $
    innerStep    0x1    0xffffffb10b1      0xc90fda8d $
    innerStep    0x2    0xfffffec42c3     0x1921fb49f $
    innerStep    0x4    0xfffffb10b0d     0x3243f655e $
    innerStep    0x8    0xffffec42c37     0x6487eabba $
    innerStep   0x10    0xffffb10b10f     0xc90fc5f66 $
    innerStep   0x20    0xfffec42c745    0x1921f0fe67 $
    innerStep   0x40    0xfffb10b4dc9    0x3243a3f9be $
    innerStep   0x80    0xffec4304267    0x648557de8e $
    innerStep  0x100    0xffb10f1bcb7    0xc8fb2f886f $
    innerStep  0x200    0xfec46d1e893   0x1917a6bc29b $
    innerStep  0x400    0xfb14be7fbae   0x31f17078d35 $
    innerStep  0x800    0xec835e79947   0x61f78a9abaa $
    innerStep 0x1000    0xb504f333f9e   0xb504f333f9e $
    innerStep 0x2000                0  0x100000000000 $
    innerStep 0x4000 (-0x100000000000)              0 $
    (i0norm, 0x100000000000, 0)
  where
    i0norm = do
        let im = i0 `mod` 0x10000
        if im <= 0x8000
            then im
            else im - 0x10000
    innerStep halfStep aCos aSin (!angle, !x, !y)
        | angle > halfStep =
            ( angle - halfStep * 2
            , (  x * aCos - y * aSin) `quot` 0x100000000000
            , (  x * aSin + y * aCos) `quot` 0x100000000000
            )
        | angle < -halfStep =
            ( angle + halfStep * 2
            , (  x * aCos + y * aSin) `quot` 0x100000000000
            , (- x * aSin + y * aCos) `quot` 0x100000000000
            )
        | otherwise =
            (angle, x, y)
    normalizeResult (_, !x, !y) =
        (x % 0x100000000000, y % 0x100000000000)

rotationMat ::
    P.Rotator ->
    Mat3
rotationMat (P.Rotator pitch yaw roll) = do
    let (rc, rs) = rotation16iCosSin roll
    let (pc, ps) = rotation16iCosSin pitch
    let (yc, ys) = rotation16iCosSin yaw
    let rmat = Mat3
            1     0    0
            0    rc   rs
            0  (-rs)  rc
    let pmat = Mat3
            pc    0   ps
            0     1    0
          (-ps)   0   pc
    let ymat = Mat3
            yc   ys    0
          (-ys)  yc    0
             0    0    1
    ymat *! pmat *! rmat

instanceXform ::
    P.InstanceTransform ->
    Xform3
instanceXform P.InstanceTransform {..} =
    Xform3
        (toVec3 itLocation)
        (   scaleMat itPostScale *!
            rotationMat itRotation *!
            scaleMat itMainScale
        )
  *!
    Xform3
        (negateVec $ toVec3 itPrePivot)
        Mat3Identity

instanceVertexSet ::
    P.Instance ->
    RTree.RTree Vec3
instanceVertexSet P.Instance {..} =
    RTree.fromList $
    Set.toList $
    foldMap
        (foldMap (Set.singleton . toVec3) . P.fVertices)
        iFaces

vertexSegmentProjection ::
    Vec3 ->
    Vec3 ->
    Vec3 ->
    Arg Rational (Vec3, Rational)
vertexSegmentProjection a b v = do
    let ab = b -. a
    let av = v -. a
    let t = dot ab av / dotSqr ab
    let avpar = t *. ab
    let avperp = av -. avpar
    Arg t (v, dotSqr avperp)

segmentTJunctionBreaks ::
    RTree.RTree Vec3 ->
    Vec3 ->
    Vec3 ->
    Seq.Seq Vec3
segmentTJunctionBreaks vertexSet a b = do
    let abBox = hull (endpointBBox a) (endpointBBox b)
    let projections =
            Seq.unstableSort $
            Seq.filter
                (\(Arg t (_, distSqr)) ->
                    t > 0 &&
                    t < 1 &&
                    distSqr <= tJunctionThreshold * tJunctionThreshold
                ) $
            RTree.foldMapIntersecting
                (overlaps abBox)
                (Seq.singleton . vertexSegmentProjection a b)
                vertexSet
    fmap (\(Arg _ (v, _)) -> v) projections
  where
    endpointBBox (Vec3 x y z) =
        BBox
            (Interval (x - tJunctionThreshold) (x + tJunctionThreshold))
            (Interval (y - tJunctionThreshold) (y + tJunctionThreshold))
            (Interval (z - tJunctionThreshold) (z + tJunctionThreshold))

breakContourOnTJunctions ::
    RTree.RTree Vec3 ->
    Seq.Seq Vec3 ->
    Seq.Seq Vec3
breakContourOnTJunctions vertexSet = do
    CSeq.foldMapPairsLoop $ \a b ->
        Seq.singleton a <> segmentTJunctionBreaks vertexSet a b

makeMaterialTransformed ::
    Xform3 ->
    P.PolysFace ->
    Material
makeMaterialTransformed instXformInverse P.PolysFace {..} =
    Material
        { materialTextureSpace = uvnXform *! instXformInverse
        , materialTextureName = fTextureName
        , materialSemisolid = fFlags .&. 0x20 /= 0
        , materialMasked = fFlags .&. 0x2 /= 0
        , materialTranslucent = fFlags .&. 0x4 /= 0
        , materialModulated = fFlags .&. 0x40 /= 0
        , materialUnlit = fFlags .&. 0x400000 /= 0
        , materialMirror = fFlags .&. 0x8000000 /= 0
        , materialInvisible = fFlags .&. 0x1 /= 0
        }
  where
    uvnMat =
        mat3Transpose $
            Mat3Cols (toVec3 fTextureU) (toVec3 fTextureV) (toVec3 fNormal)
    uvnXform = Xform3 (uvnMat *! toVec3 fOrigin) uvnMat

splitVertices ::
    [Vec3] ->
    [Seq Vec3]
splitVertices (v0 : v1 : v2 : vrest)
    | cross3 (v1 -. v0) (v2 -. v0) /= Vec3 0 0 0 =
        (Seq.fromList [v0, v1, v2]) : splitVertices (v0 : v2 : vrest)
    | otherwise =
        splitVertices (v0 : v2 : vrest)
splitVertices _ = []

makeFacesTransformed ::
    RTree.RTree Vec3 ->
    Xform3 ->
    P.PolysFace ->
    [Face Material]
makeFacesTransformed vertexSet instXform pface = do
    let instXformInverse =
            case xform3Inverse instXform of
                Nothing -> Xform3Identity
                Just x -> x
    let mat = makeMaterialTransformed instXformInverse pface
    let patches =
            splitVertices $ toList $
                fmap (snapVec3 . (instXform *!)) $
                    breakContourOnTJunctions vertexSet $
                        fmap toVec3 (P.fVertices pface)
    map (\vs -> makeFace vs mat) patches

instanceBrush ::
    P.Instance ->
    Brush (WithBrushSides Material) (WithSideAtInfinity ())
instanceBrush inst = do
    let instXform = instanceXform (P.iTransform inst)
    let vertexSet = instanceVertexSet inst
    let faces =
            concatMap
                (makeFacesTransformed vertexSet instXform)
                (P.iFaces inst)
    let pendingBrush = List.foldl' insertFace (emptyBrush ()) faces
    markBrushSides pendingBrush

brushApplyOper ::
    P.InstanceOper ->
    Brush (WithBrushSides Material) (WithSideAtInfinity ()) ->
    Brush (WithBrushSides Material) (WithSideAtInfinity ()) ->
    Brush (WithBrushSides Material) (WithSideAtInfinity ())
brushApplyOper oper =
    case oper of
        P.OperAddSolid -> brushOperAddSolid
        P.OperSubtractSolid -> brushOperSubtractSolid
        P.OperAddNonSolid -> brushOperAddNonSolid
        P.OperAddSemiSolid -> brushOperAddSemiSolid

insertInstance ::
    Brush (WithBrushSides Material) (WithSideAtInfinity ()) ->
    P.Instance ->
    Brush (WithBrushSides Material) (WithSideAtInfinity ())
insertInstance brush inst =
    brushApplyOper (P.iOper inst) brush (instanceBrush inst)

data InstanceEvent = InstanceEvent
    { ieCurrentNumber :: Int
    , ieTotalNumber :: Int
    , ieObjectName :: Text.Text
    , ieWorldBrush :: Brush (WithBrushSides Material) (WithSideAtInfinity ())
    , ieInstance :: P.Instance
    }

importWorld ::
    (Monad m) =>
    (InstanceEvent -> m ()) ->
    Brush (WithBrushSides Material) (WithSideAtInfinity ()) ->
    Seq P.Instance ->
    m (Brush (WithBrushSides Material) (WithSideAtInfinity ()))
importWorld onInstance genesisBrush instList = do
    let (semis, nonSemis) = Seq.partition P.iIsSemisolid instList
    (_, finalWorld) <-
        foldlM
            (\(i, currentWorld) inst -> do
                onInstance $ InstanceEvent
                    { ieCurrentNumber = i
                    , ieTotalNumber = Seq.length instList
                    , ieObjectName = P.iObjectName inst
                    , ieWorldBrush = currentWorld
                    , ieInstance = inst
                    }
                let !nextWorld = insertInstance currentWorld inst
                pure (i+1, nextWorld)
            )
            (1, genesisBrush)
            (nonSemis <> semis)
    pure finalWorld

importWorldToFull ::
    (Monad m) =>
    (InstanceEvent -> m ()) ->
    Seq P.Instance ->
    m (Brush (WithBrushSides Material) (WithSideAtInfinity ()))
importWorldToFull onInstance = importWorld onInstance (fullWorld ())
