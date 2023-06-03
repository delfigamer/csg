module Export.Bvh where

import Common.Types
import Control.Monad.State
import Data.Bits
import Data.Foldable
import Data.Int
import Data.Word
import Geometry.Brush
import Geometry.Class
import Geometry.Mat3
import Geometry.Triangle
import Geometry.Vec3
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS.Builder
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Map as Map
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Geometry.RTree as RTree

type RTree4 = RTree.RTreeT ('RTree.RTreeParams 2 4 4)

newtype MaterialIndex = MaterialIndex Int32

newtype MaterialStringIndex = MaterialStringIndex Int32

data WithOtherZoneNumber a = WithOtherZoneNumber Int32 a

data WithNormalIndex a = WithNormalIndex Int32 a

data IndexTriangle a = IndexTriangle Int32 Int32 Int32 a

magic :: Word64
magic = 0xdb7652c5014bac40

exportBvh ::
    WithZoneAtInfinity () ->
    Seq.Seq (Triangle (WithZoneNumbers Material)) ->
    BS.Lazy.ByteString
exportBvh (WithZoneAtInfinity zinf _) faceSeq =
    BS.Builder.toLazyByteString $ serializeWorld zinf faceSeq

serializeWorld ::
    Int ->
    Seq.Seq (Triangle (WithZoneNumbers Material)) ->
    BS.Builder.Builder
serializeWorld zinf triSeq =
    BS.Builder.word64LE magic <>
    serializeStringList stringSeq <>
    serializeMaterialList matSeqStripped <>
    serializeVertexList vertexSeq <>
    serializeNormalList normSeq <>
    serializeInt (Map.size triangleTreeStrippedZoneMap) <>
    Map.foldMapWithKey
        (\zoneId tree ->
            serializeZoneFlags (zoneId == zinf) <>
            serializeTriangleTree tree
        )
        triangleTreeStrippedZoneMap
  where
    (triangleTreeStrippedZoneMap, IndexTable vertexSeq _) =
        runState
            (traverse collectVertices triangleTreeZoneMap)
            emptyIndexTable
    triangleTreeZoneMap =
        fmap (foldl' RTree.append RTree.empty) triSeqStrippedZoneMapWithNorms
    (triSeqStrippedZoneMapWithNorms, IndexTable normSeq _) =
        runState
            (traverse collectNormals triSeqStrippedZoneMap)
            emptyIndexTable
    triSeqStrippedZoneMap =
        splitByZones triSeqStripped
    (matSeqStripped, IndexTable stringSeq _) =
        runState (collectMaterialStrings matSeq) emptyIndexTable
    (triSeqStripped, IndexTable matSeq _) =
        runState (collectMaterials triSeq) emptyIndexTable

splitByZones ::
    Seq.Seq (Triangle (WithZoneNumbers a)) ->
    Map.Map Int (Seq.Seq (Triangle (WithOtherZoneNumber a)))
splitByZones triSeq =
    MonoidMap.unwrapMonoidMap $
        foldMap onTriangle triSeq
  where
    onTriangle (Triangle p1 p2 p3 bbox (WithZoneNumbers zb zf otherData)) =
        MonoidMap.singleton
            zf
            (Seq.singleton $
                Triangle p1 p2 p3 bbox
                    (WithOtherZoneNumber (fromIntegral zb) otherData)
            ) <>
        MonoidMap.singleton
            zb
            (Seq.singleton $
                Triangle p3 p2 p1 bbox
                    (WithOtherZoneNumber (fromIntegral zf) otherData)
            )

data IndexTable b = IndexTable
    { indexTableSeq :: Seq.Seq b
    , indexTableMap :: Map.Map b Int32
    }

emptyIndexTable :: IndexTable b
emptyIndexTable = IndexTable Seq.empty Map.empty

upsertIndexTable ::
    (Ord b) =>
    b ->
    IndexTable b ->
    (Int32, IndexTable b)
upsertIndexTable x oldIT@(IndexTable valueSeq indexMap) =
    case Map.lookup x indexMap of
        Nothing ->
            if length valueSeq >= 0x7fffffff
                then error $ "too many elements in a table"
                else do
                    let newIndex = fromIntegral $ length valueSeq
                    let newValueSeq = valueSeq Seq.:|> x
                    let newIndexMap = Map.insert x newIndex indexMap
                    (newIndex, (IndexTable newValueSeq newIndexMap))
        Just index ->
            (index, oldIT)

compactZoneNumbers ::
    Seq.Seq (Triangle (WithZoneNumbers a)) ->
    Seq.Seq (Triangle (WithZoneNumbers a))
compactZoneNumbers triSeq =
    fst $ runState (collectZones triSeq) emptyIndexTable
  where
    collectZones =
        traverse $
            traverse $ \ (WithZoneNumbers zb zf otherData) -> do
                zb2 <- State $ upsertIndexTable zb
                zf2 <- State $ upsertIndexTable zf
                pure $ WithZoneNumbers
                    (fromIntegral zb2)
                    (fromIntegral zf2)
                    otherData

collectMaterials ::
    Seq.Seq (Triangle (WithZoneNumbers Material)) ->
    State
        (IndexTable Material)
        (Seq.Seq (Triangle (WithZoneNumbers MaterialIndex)))
collectMaterials =
    traverse $
        traverse $
            traverse $ \mat -> do
                MaterialIndex <$> (State $ upsertIndexTable mat)

collectMaterialStrings ::
    Seq.Seq Material ->
    State (IndexTable Text.Text) (Seq.Seq (MaterialT MaterialStringIndex))
collectMaterialStrings =
    traverse $ \mat ->
        State $ \textTable -> do
            case upsertIndexTable (materialTextureName mat) textTable of
                (index, newTextTable) ->
                    ( mat {materialTextureName = MaterialStringIndex index}
                    , newTextTable
                    )

collectNormals ::
    Seq.Seq (Triangle a) ->
    State (IndexTable IVec3) (Seq.Seq (Triangle (WithNormalIndex a)))
collectNormals =
    traverse $ \tri -> do
        let ab = triPoint2 tri -. triPoint1 tri
        let ac = triPoint3 tri -. triPoint1 tri
        let n = cross3 ab ac
        let ni = vec3ToIntegerScaled n
        index <- State $ upsertIndexTable ni
        pure $ fmap (WithNormalIndex index) tri

collectVertices ::
    RTree4 (Triangle a) ->
    State (IndexTable Vec3) (RTree4 (IndexTriangle a))
collectVertices =
    RTree.traverseStable $ \tri -> do
        index1 <- State $ upsertIndexTable (triPoint1 tri)
        index2 <- State $ upsertIndexTable (triPoint2 tri)
        index3 <- State $ upsertIndexTable (triPoint3 tri)
        pure (IndexTriangle index1 index2 index3 $ triData tri)

serializeTriangleTree ::
    RTree4 (IndexTriangle (WithNormalIndex (WithOtherZoneNumber MaterialIndex))) ->
    BS.Builder.Builder
serializeTriangleTree =
    maybe mempty id .
    RTree.foldStructured
        (\tri -> "\0\0\0\0" <> serializeTriangle tri)
        (\height _bbox children ->
            serializeInt height <>
            serializeInt (length children) <>
            fold children
        )

serializeTriangle ::
    (IndexTriangle (WithNormalIndex (WithOtherZoneNumber MaterialIndex))) ->
    BS.Builder.Builder
serializeTriangle
    (IndexTriangle index1 index2 index3
        (WithNormalIndex normIndex
            (WithOtherZoneNumber otherZone
                (MaterialIndex matIndex)
            )
        )
    )
  =
    BS.Builder.int32LE index1 <>
    BS.Builder.int32LE index2 <>
    BS.Builder.int32LE index3 <>
    BS.Builder.int32LE normIndex <>
    BS.Builder.int32LE matIndex <>
    BS.Builder.int32LE otherZone

serializeVertexList ::
    Seq.Seq Vec3 ->
    BS.Builder.Builder
serializeVertexList vertexSeq =
    serializeInt (length vertexSeq) <>
    foldMap serializeVec3 vertexSeq

serializeNormalList ::
    Seq.Seq IVec3 ->
    BS.Builder.Builder
serializeNormalList normalSeq =
    serializeInt (length normalSeq) <>
    foldMap serializeIVec3Normalized normalSeq

serializeMaterialList ::
    Seq.Seq (MaterialT MaterialStringIndex) ->
    BS.Builder.Builder
serializeMaterialList matSeq =
    serializeInt (length matSeq) <>
    foldMap serializeMaterial matSeq

serializeZoneFlags ::
    Bool ->
    BS.Builder.Builder
serializeZoneFlags isZoneAtInfinity =
    BS.Builder.word32LE $
        flagValue 1 isZoneAtInfinity

{- 56 bytes -}
serializeMaterial ::
    MaterialT MaterialStringIndex ->
    BS.Builder.Builder
serializeMaterial mat =
    BS.Builder.int32LE matNameIndex <>
    BS.Builder.word32LE flags <>
    serializeXform3 (materialTextureSpace mat)
  where
    MaterialStringIndex matNameIndex = materialTextureName mat
    flags =
        flagValue 1 (materialMasked mat) .|.
        flagValue 2 (materialTranslucent mat) .|.
        flagValue 4 (materialModulated mat) .|.
        flagValue 8 (materialUnlit mat) .|.
        flagValue 16 (materialMirror mat) .|.
        flagValue 32 (materialInvisible mat)

{- 48 bytes -}
serializeXform3 ::
    Xform3 ->
    BS.Builder.Builder
serializeXform3 (Xform3 offset (Mat3Cols xc yc zc)) =
    serializeVec3 offset <>
    serializeVec3 xc <>
    serializeVec3 yc <>
    serializeVec3 zc

{- 12 bytes -}
serializeIVec3Normalized ::
    IVec3 ->
    BS.Builder.Builder
serializeIVec3Normalized (IVec3 ix iy iz) =
    BS.Builder.floatLE fnx <>
    BS.Builder.floatLE fny <>
    BS.Builder.floatLE fnz
  where
    fnx = fx / fnorm
    fny = fy / fnorm
    fnz = fz / fnorm
    fnorm = sqrt (fx*fx + fy*fy + fz*fz)
    fx = fromInteger ix
    fy = fromInteger iy
    fz = fromInteger iz

{- 12 bytes -}
serializeVec3 ::
    Vec3 ->
    BS.Builder.Builder
serializeVec3 (Vec3 x y z) =
    BS.Builder.floatLE (fromRational x) <>
    BS.Builder.floatLE (fromRational y) <>
    BS.Builder.floatLE (fromRational z)

serializeStringList ::
    Seq.Seq Text.Text ->
    BS.Builder.Builder
serializeStringList stringSeq =
    serializeInt (length stringSeq) <>
    foldMap serializeString stringSeq

serializeString ::
    Text.Text ->
    BS.Builder.Builder
serializeString str =
    serializeInt (BS.length bytes) <>
    BS.Builder.byteString bytes
  where
    bytes = Text.Encoding.encodeUtf8 str

serializeInt ::
    Int ->
    BS.Builder.Builder
serializeInt i
    | -0x80000000 <= i && i <= 0x7fffffff =
        BS.Builder.int32LE (fromIntegral i)
    | otherwise =
        error $ "integer is too large: " <> show i

flagValue :: Word32 -> Bool -> Word32
flagValue x b
    | b = x
    | otherwise = 0
