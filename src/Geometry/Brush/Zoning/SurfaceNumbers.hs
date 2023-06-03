module Geometry.Brush.Zoning.SurfaceNumbers where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Foldable
import Data.Interval
import Data.Ratio
import Data.Semigroup
import Data.Sequence (Seq (..))
import Geometry.AngleRat
import Geometry.Brush.Type
import Geometry.Class
import Geometry.Face
import Geometry.Vec3
import qualified Data.CSeq as CSeq
import qualified Data.DisjointSet as DS
import qualified Data.DisjointSet.WithData as DSData
import qualified Data.IntervalMap as IntervalMap
import qualified Data.MonoidMap as MonoidMap
import qualified Data.MultiSet as MultiSet
import qualified Data.Vector as Vector
import qualified Geometry.RTree as RTree

data WithSurfaceNumbers a =
    WithSurfaceNumbers
        {-# UNPACK #-} !Int {- back side -}
        {-# UNPACK #-} !Int {- front side -}
        {-# UNPACK #-} !Int {- connectivity group -}
        a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (ToWolfMod a) => ToWolfMod (WithSurfaceNumbers a) where
    toWolfMod (WithSurfaceNumbers bs fs cg a) =
        toWolfMod a ++
        [ wolfApp "FaceForm"
            [ wolfApp "textTexture"
                [toWolf $ show cg ++ "/" ++ show fs]
            , wolfApp "textTextureReflect"
                [toWolf $ show cg ++ "/" ++ show bs]
            ]
        ]

type FaceXAxisOrderArg a =
    Min (Arg (Rational, Rational) (Face a))

type FaceXAxisOrderArgVector a =
    Vector.Vector (FaceXAxisOrderArg (WithSurfaceNumbers a))

numberBrushSurfaces ::
    (Show a) =>
    Brush a b ->
    ST s (Brush (WithSurfaceNumbers a) b, DS.DisjointSet s)
numberBrushSurfaces originalBrush = do
    let (surfCount, faceOrderArgVector, brushWithNumbers) =
            numberBrushFaceSurfaces originalBrush
    dsetSurf <- DS.newDisjointSet surfCount
    dsetGroup <- DSData.newDisjointSet faceOrderArgVector
    unifyAdjacentSurfaceNumbers
        (constructEdgeMap brushWithNumbers)
        dsetSurf dsetGroup
    unifyNonConnectedZones
        brushWithNumbers
        dsetSurf dsetGroup
    pure (brushWithNumbers, dsetSurf)

numberBrushFaceSurfaces ::
    Brush a b ->
    (Int, FaceXAxisOrderArgVector a, Brush (WithSurfaceNumbers a) b)
numberBrushFaceSurfaces brush =
    case
        runState
            (RTree.traverseStable processFace (brushFaces brush))
            (1, 0, Empty)
      of
        (modifiedFaces, (surfCounter, groupCounter, args)) -> do
            let newBrush =
                    brush
                        { brushFaces = modifiedFaces
                        }
            let argsVector = Vector.fromListN groupCounter (toList args)
            (surfCounter, argsVector, newBrush)
  where
    processFace face = State $ \(surfCounter, groupCounter, args) -> do
        let numbers =
                WithSurfaceNumbers
                    surfCounter
                    (surfCounter + 1)
                    groupCounter
        let modifiedFace = fmap numbers face
        let !(minx, acs) = faceXAxisOrder face
        let newArg = Min (Arg (minx, acs)  modifiedFace)
        (modifiedFace, (surfCounter + 2, groupCounter + 1, args :|> newArg))

type BrushEdgeMap =
    MonoidMap.MonoidMap
        Axis3
        (IntervalMap.IntervalMap
            Rational
            (MultiSet.MultiSet (AngleRat, Int, Int, Int))
        )

constructEdgeMap ::
    Brush (WithSurfaceNumbers a) b ->
    BrushEdgeMap
constructEdgeMap brush =
    foldMap onFace (brushFaces brush)
  where
    onFace ::
        Face (WithSurfaceNumbers a) ->
        BrushEdgeMap
    onFace face =
        CSeq.foldMapPairsLoop (onEdge face) (facePoints face)
    onEdge ::
        Face (WithSurfaceNumbers a) ->
        Vec3 ->
        Vec3 ->
        BrushEdgeMap
    onEdge face a b = do
        let (invertedDir, axis@(Axis3 _ axisDir _)) = makeAxis3Between a b
        let aarg = axis3Dot axis a
        let barg = axis3Dot axis b
        let argInterval =
                if invertedDir
                    then Interval barg aarg
                    else Interval aarg barg
        let angle1 =
                angleRat3i
                    axisDir
                    (orthogonalVec3i axisDir)
                    (faceNormal face)
        let angle =
                if invertedDir
                    then negateAngleRat angle1
                    else angle1
        let (sn1, sn2, cg) =
                case faceData face of
                    WithSurfaceNumbers snb snf cg' _ ->
                        if invertedDir
                            then (snf, snb, cg')
                            else (snb, snf, cg')
        MonoidMap.singleton axis $
            IntervalMap.singleton argInterval $
                MultiSet.singleton (angle, sn1, sn2, cg)

unifyAdjacentSurfaceNumbers ::
    BrushEdgeMap ->
    DS.DisjointSet s ->
    DSData.DisjointSet s (FaceXAxisOrderArg (WithSurfaceNumbers a)) ->
    ST s ()
unifyAdjacentSurfaceNumbers edgeMap dsetSurf dsetGroup = do
    forM_ edgeMap $ \lineIntervalMap -> do
        let intervalList = IntervalMap.toIntervalList lineIntervalMap
        forM_ intervalList $ \(_, surfMultiSet) -> do
            let surfSeq = MultiSet.toSeq surfMultiSet
            CSeq.forPairsLoopM_ surfSeq $ \(_, _, sn1, cg1) (_, sn2, _, cg2) -> do
                DS.unifySubsets dsetSurf sn1 sn2
                DSData.unifySubsets dsetGroup cg1 cg2

faceXAxisOrder ::
    Face a ->
    (Rational, Rational)
faceXAxisOrder face = do
    let !(HalfSpace n@(IVec3 nx _ _) _) = faceHalfSpace face
    let minx =
            minimum $
                fmap
                    (\(Vec3 x _ _) -> x)
                    (facePoints face)
    let angleCosSqr = 1 - nx*nx % dotSqr n
    (minx, angleCosSqr)

unifyNonConnectedZones ::
    (Show a) =>
    Brush (WithSurfaceNumbers a) b ->
    DS.DisjointSet s ->
    DSData.DisjointSet s (FaceXAxisOrderArg (WithSurfaceNumbers a)) ->
    ST s ()
unifyNonConnectedZones brush dsetSurf dsetGroup = do
    groupRoots <- DSData.collectRoots dsetGroup
    forM_ groupRoots $ \(_, Min (Arg _ face)) -> do
        case brushXAxisRaycast (faceCenter face) brush of
            Nothing -> do
                DS.unifySubsets
                    dsetSurf
                    (surfaceNumberAtBack face)
                    0
            Just hitFace -> do
                DS.unifySubsets
                    dsetSurf
                    (surfaceNumberAtBack face)
                    (surfaceNumberAtFront hitFace)
  where
    surfaceNumberAtFront face =
        case faceData face of
            WithSurfaceNumbers _ snf _ _ ->
                snf
    surfaceNumberAtBack face =
        case faceData face of
            WithSurfaceNumbers snb _ _ _ ->
                snb
