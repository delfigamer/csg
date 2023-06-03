module Geometry.Brush.Merging where

import Data.Interval
import Geometry.BBox
import Control.Monad
import Geometry.Brush.Type
import Geometry.Brush.Zoning
import Geometry.Face
import Geometry.Vec3
import qualified Geometry.RTree as RTree

data MergeStrategyForInterior at av
    = MergeStrategyForInteriorDrop
    | (at ~ av) => MergeStrategyForInteriorKeep

type MergeHandleInterior at av =
    BrushSide ->
    MergeStrategyForInterior at av

applyMergeHandleInteriorAtTree ::
    BrushSide ->
    MergeHandleInterior at av ->
    RTree.RTree (Face (WithBrushSides at)) ->
    RTree.RTree (Face (WithBrushSides av))
applyMergeHandleInteriorAtTree bside onFaceA atree = do
    case onFaceA bside of
        MergeStrategyForInteriorDrop ->
            RTree.empty
        MergeStrategyForInteriorKeep ->
            atree

applyMergeHandleInteriorAtFace ::
    BrushSide ->
    MergeHandleInterior at av ->
    Face (WithBrushSides at) ->
    RTree.RTree (Face (WithBrushSides av))
applyMergeHandleInteriorAtFace bside onFaceA aface = do
    case onFaceA bside of
        MergeStrategyForInteriorDrop ->
            RTree.empty
        MergeStrategyForInteriorKeep ->
            RTree.singleton aface

type MergeHandleOverlap at au av =
    WithBrushSides at ->
    WithBrushSides au ->
    Maybe (WithBrushSides av)

data WithMaybeOverlap t a =
    WithMaybeOverlap (Maybe t) a
    deriving (Show)

generalizedMergeBrushes ::
    (Show at, Show au, Show av) =>
    (WithSideAtInfinity bt -> WithSideAtInfinity bu -> WithSideAtInfinity bv) ->
    MergeHandleInterior at av ->
    MergeHandleInterior au av ->
    MergeHandleOverlap at au av ->
    Brush (WithBrushSides at) (WithSideAtInfinity bt) ->
    Brush (WithBrushSides au) (WithSideAtInfinity bu) ->
    Brush (WithBrushSides av) (WithSideAtInfinity bv)
generalizedMergeBrushes mergeData onFaceT onFaceU onOverlap tbrush ubrush = do
    let tbrushFacesProcessed =
            RTree.concatMapIntersectingTree
                (\tsubtree tbox ->
                    applyMergeHandleInteriorAtTree
                        (brushSideAt (bboxRepresentativePoint tbox) ubrush)
                        onFaceT
                        tsubtree
                )
                (\tface usubtree -> do
                    let tfacePieces =
                            splitFaceWithTree tface (\_ _ -> []) usubtree
                    foldMap
                        (\tpiece ->
                            applyMergeHandleInteriorAtFace
                                (brushSideAt (faceCenter tpiece) ubrush)
                                onFaceT
                                tpiece
                        )
                        tfacePieces
                )
                (brushFaces tbrush)
                (brushFaces ubrush)
    let ubrushFacesProcessed =
            RTree.concatMapIntersectingTree
                (\usubtree ubox ->
                    applyMergeHandleInteriorAtTree
                        (brushSideAt (bboxRepresentativePoint ubox) tbrush)
                        onFaceU
                        usubtree
                )
                (\uface tsubtree -> do
                    let ufacePieces =
                            splitFaceWithTree
                                (fmap (WithMaybeOverlap Nothing) uface)
                                (\uOverlappingPiece tOverlappingFace ->
                                    [ fmap
                                        (\(WithMaybeOverlap _ udata) ->
                                            WithMaybeOverlap
                                                (Just $ faceData tOverlappingFace)
                                                udata
                                        )
                                        uOverlappingPiece
                                    ]
                                )
                                tsubtree
                    foldMap
                        (\upiece ->
                            case faceData upiece of
                                WithMaybeOverlap Nothing udata ->
                                    applyMergeHandleInteriorAtFace
                                        (brushSideAt (faceCenter upiece) tbrush)
                                        onFaceU
                                        (udata <$ upiece)
                                WithMaybeOverlap (Just tdata) udata ->
                                    case onOverlap tdata udata of
                                        Nothing -> RTree.empty
                                        Just vdata ->
                                            RTree.singleton $
                                                vdata <$ upiece
                        )
                        ufacePieces
                )
                (brushFaces ubrush)
                (brushFaces tbrush)
    Brush
        { brushFaces = tbrushFacesProcessed <> ubrushFacesProcessed
        , brushData = mergeData (brushData tbrush) (brushData ubrush)
        }

bboxRepresentativePoint ::
    BBox ->
    Vec3
bboxRepresentativePoint (BBox (Interval x1 _) (Interval y1 _) (Interval z1 _)) =
    Vec3 x1 y1 z1

brushSideAt ::
    Vec3 ->
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    BrushSide
brushSideAt origin brush = do
    case brushXAxisRaycast origin brush of
        Nothing ->
            case brushData brush of
                WithSideAtInfinity infSide _ ->
                    infSide
        Just hitFace ->
            case faceData hitFace of
                WithBrushSides _ frontSide _ ->
                    frontSide

removeInteriorFaces ::
    (Show a) =>
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
removeInteriorFaces brush =
    brush
        { brushFaces =
            foldMap
                (\face ->
                    case faceData face of
                        WithBrushSides Inside Inside _ ->
                            RTree.empty
                        _ ->
                            RTree.singleton face
                )
                (brushFaces brush)
        }

makeBrushHollow ::
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
makeBrushHollow brush =
    Brush
        { brushFaces =
            RTree.mapStable
                (fmap hollowFaceData)
                (brushFaces brush)
        , brushData =
            hollowBrushData $ brushData brush
        }
  where
    hollowFaceData (WithBrushSides _ _ otherData) =
        WithBrushSides Outside Outside otherData
    hollowBrushData (WithSideAtInfinity _ otherData) =
        WithSideAtInfinity Outside otherData

invertBrush ::
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
invertBrush brush =
    Brush
        { brushFaces =
            RTree.mapStable
                (fmap invertFaceData)
                (brushFaces brush)
        , brushData =
            invertBrushData $ brushData brush
        }
  where
    invertFaceData (WithBrushSides bside fside otherData) =
        WithBrushSides (invertSide bside) (invertSide fside) otherData
    invertBrushData (WithSideAtInfinity iside otherData) =
        WithSideAtInfinity (invertSide iside) otherData
    invertSide Inside = Outside
    invertSide Outside = Inside

unionSide :: BrushSide -> BrushSide -> BrushSide
unionSide Inside _ = Inside
unionSide Outside s = s

intersectSide :: BrushSide -> BrushSide -> BrushSide
intersectSide Outside _ = Outside
intersectSide Inside s = s

brushOperAddSolid ::
    (Show a) =>
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
brushOperAddSolid tbrush ubrush =
    generalizedMergeBrushes
        mergeBrushData onFaceT onFaceU onOverlap
        tbrush (removeInteriorFaces ubrush)
  where
    mergeBrushData
        (WithSideAtInfinity tiside _)
        (WithSideAtInfinity uiside udata)
      =
        WithSideAtInfinity (unionSide tiside uiside) udata
    onFaceT = \case
        Inside -> MergeStrategyForInteriorDrop
        Outside -> MergeStrategyForInteriorKeep
    onFaceU = \case
        Inside -> MergeStrategyForInteriorDrop
        Outside -> MergeStrategyForInteriorKeep
    onOverlap
        (WithBrushSides tbside tfside _)
        (WithBrushSides ubside ufside udata)
      = do
        let vbside = unionSide tbside ubside
        let vfside = unionSide tfside ufside
        guard ((vbside, vfside) /= (Inside, Inside))
        Just (WithBrushSides vbside vfside udata)

brushOperIntersectSolid ::
    (Show a) =>
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
brushOperIntersectSolid tbrush ubrush =
    generalizedMergeBrushes
        mergeBrushData onFaceT onFaceU onOverlap
        tbrush ubrush
  where
    mergeBrushData
        (WithSideAtInfinity tiside _)
        (WithSideAtInfinity uiside udata)
      =
        WithSideAtInfinity (intersectSide tiside uiside) udata
    onFaceT = \case
        Inside -> MergeStrategyForInteriorKeep
        Outside -> MergeStrategyForInteriorDrop
    onFaceU = \case
        Inside -> MergeStrategyForInteriorKeep
        Outside -> MergeStrategyForInteriorDrop
    onOverlap
        (WithBrushSides tbside tfside _)
        (WithBrushSides ubside ufside udata)
      = do
        let vbside = intersectSide tbside ubside
        let vfside = intersectSide tfside ufside
        guard ((vbside, vfside) /= (Outside, Outside))
        Just (WithBrushSides vbside vfside udata)

brushOperSubtractSolid ::
    (Show a) =>
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
brushOperSubtractSolid tbrush ubrush =
    brushOperIntersectSolid tbrush (invertBrush (removeInteriorFaces ubrush))

brushOperAddNonSolid ::
    (Show a) =>
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
brushOperAddNonSolid tbrush ubrush =
    generalizedMergeBrushes
        mergeBrushData onFaceT onFaceU onOverlap
        tbrush (makeBrushHollow ubrush)
  where
    mergeBrushData
        (WithSideAtInfinity tiside _)
        (WithSideAtInfinity _ udata)
      =
        WithSideAtInfinity tiside udata
    onFaceT = \case
        _ -> MergeStrategyForInteriorKeep
    onFaceU = \case
        Inside -> MergeStrategyForInteriorDrop
        Outside -> MergeStrategyForInteriorKeep
    onOverlap
        (WithBrushSides tbside tfside _)
        (WithBrushSides _ _ udata)
      = do
        Just (WithBrushSides tbside tfside udata)

brushOperAddSemiSolid ::
    (Show a) =>
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b) ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
brushOperAddSemiSolid tbrush ubrush =
    generalizedMergeBrushes
        mergeBrushData onFaceT onFaceU onOverlap
        tbrush (makeBrushHollow ubrush)
  where
    mergeBrushData
        (WithSideAtInfinity tiside _)
        (WithSideAtInfinity _ udata)
      =
        WithSideAtInfinity tiside udata
    onFaceT = \case
        _ -> MergeStrategyForInteriorKeep
    onFaceU = \case
        Inside -> MergeStrategyForInteriorDrop
        Outside -> MergeStrategyForInteriorKeep
    onOverlap
        (WithBrushSides tbside tfside tdata)
        _
      = do
        Just (WithBrushSides tbside tfside tdata)
