module Geometry.Brush.Type where

import Data.Foldable
import Data.Interval
import Data.Semigroup
import Geometry.BBox
import Geometry.Class
import Geometry.Face
import Geometry.Vec3
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Geometry.RTree as RTree

data Brush a b = Brush
    { brushFaces :: !(RTree.RTree (Face a))
    , brushData :: b
    }
  deriving (Show, Functor, Foldable, Traversable)

Aeson.deriveJSON Aeson.defaultOptions ''Brush

instance (ToWolfMod a, ToWolfMod b) => ToWolf (Brush a b) where
    toWolf brush =
        wolfList $
            wfdata ++
            toList (RTree.foldStructured onLeaf onBranch (brushFaces brush))
      where
        wfdata = toWolfMod (brushData brush)
        onLeaf face =
            toWolf face
        onBranch depth bbox elems =
            wolfList $ toList elems ++ [wfbox depth bbox]
        wfbox depth (BBox (Interval x1 x2) (Interval y1 y2) (Interval z1 z2)) =
            wolfApp "bbox"
                [ toWolf depth
                , toWolf $ Vec3 x1 y1 z1
                , toWolf $ Vec3 x2 y2 z2
                ]

-- mapBrushFaces ::
    -- (Face a1 -> Face a2) ->
    -- Brush a1 b ->
    -- Brush a2 b
-- mapBrushFaces f brush = brush {brushFaces = RTree.map f (brushFaces brush)}

emptyBrush :: b -> Brush a b
emptyBrush bd =
    Brush
        { brushFaces = RTree.empty
        , brushData = bd
        }

splitTreeWithFace ::
    (Show a) =>
    RTree.RTree (Face a) ->
    (Face a -> Face b -> [Face a]) ->
    Face b ->
    RTree.RTree (Face a)
splitTreeWithFace atree onOverlap bface =
    RTree.concatMapIntersecting
        (overlaps (bboxOf bface))
        (\aface -> do
            let (outerParts, mbCenterPiece) =
                    splitFace aface bface
            RTree.fromList $
                maybe [] (\x -> onOverlap x bface) mbCenterPiece ++
                outerParts
        )
        atree

splitFaceWithTree ::
    (Show a) =>
    Face a ->
    (Face a -> Face b -> [Face a]) ->
    RTree.RTree (Face b) ->
    RTree.RTree (Face a)
splitFaceWithTree aface onOverlap btree =
    RTree.foldrIntersecting
        (overlaps (bboxOf aface))
        (\bface curTree -> do
            splitTreeWithFace curTree onOverlap bface
        )
        (RTree.singleton aface)
        btree

insertFace ::
    (Show a) =>
    Brush a b ->
    Face a ->
    Brush a b
insertFace brush newFace = do
    let splitBrushFaces =
            splitTreeWithFace (brushFaces brush) (\_ _ -> []) newFace
    let newFacePieces =
            splitFaceWithTree newFace (\a _ -> [a]) (brushFaces brush)
    brush
        { brushFaces = splitBrushFaces <> newFacePieces
        }

brushXAxisRaycast ::
    Vec3 ->
    Brush a b ->
    Maybe (Face a)
brushXAxisRaycast origin brush = do
    Max (Arg _ maxFace) <-
        RTree.xAxisRaycast
            origin
            (\face -> do
                int <- intersectXRayWithFace xray face
                Just
                    ( xrayFaceIntersectionRightXPos int
                    , Max $ Arg
                        ( xrayFaceIntersectionTangentConeAngle int
                        , xrayFaceIntersectionPlaneAngle int
                        )
                        face
                    )
            )
            (brushFaces brush)
    Just maxFace
  where
    xray = xray3At origin

rebuildBrush :: (Show a) => Brush a b -> Brush a b
rebuildBrush brush =
    brush
        { brushFaces =
            RTree.fromList $ toList $ brushFaces brush
        }
