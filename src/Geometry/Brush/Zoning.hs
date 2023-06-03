module Geometry.Brush.Zoning where

import Control.Monad
import Control.Monad.ST
import Geometry.Brush.Type
import Geometry.Brush.Zoning.SurfaceNumbers
import Geometry.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.DisjointSet as DS
import qualified Data.Map.Strict as Map
import qualified Geometry.RTree as RTree

data BrushSide = Outside | Inside
  deriving (Show, Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''BrushSide

data WithBrushSides a =
    WithBrushSides
        !BrushSide {- back side -}
        !BrushSide {- front side -}
        a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

Aeson.deriveJSON Aeson.defaultOptions ''WithBrushSides

data WithSideAtInfinity b =
    WithSideAtInfinity
        !BrushSide
        b
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

Aeson.deriveJSON Aeson.defaultOptions ''WithSideAtInfinity

emptyWorld :: b -> Brush a (WithSideAtInfinity b)
emptyWorld a = emptyBrush (WithSideAtInfinity Outside a)

fullWorld :: b -> Brush a (WithSideAtInfinity b)
fullWorld a = emptyBrush (WithSideAtInfinity Inside a)

instance (ToWolfMod a) => ToWolfMod (WithBrushSides a) where
    toWolfMod (WithBrushSides bs fs a) =
        toWolfMod a ++ [wfsides]
      where
        wfsides = wolfApp "FaceForm" $
            [ wolfList [sideColor fs]
            , wolfList [sideColor bs]
            ]
        sideColor Outside = "Yellow"
        sideColor Inside = "Blue"

instance (ToWolfMod a) => ToWolfMod (WithSideAtInfinity a) where
    toWolfMod (WithSideAtInfinity _ a) = toWolfMod a

stripBrushSides ::
    Brush (WithBrushSides a) (WithSideAtInfinity b) -> Brush a b
stripBrushSides brush =
    Brush
        { brushFaces =
            RTree.mapStable
                (fmap $ \(WithBrushSides _ _ otherData) -> otherData)
                (brushFaces brush)
        , brushData =
            case brushData brush of
                WithSideAtInfinity _ otherData -> otherData
        }

markBrushSides ::
    (Show a) =>
    Brush a b ->
    Brush (WithBrushSides a) (WithSideAtInfinity b)
markBrushSides originalBrush = do
    runST $ do
        (brushWithNumbers, dsetSurf) <- numberBrushSurfaces originalBrush
        newBrush <- markBrushSidesByNumbers brushWithNumbers dsetSurf
        pure (fmap (WithSideAtInfinity Outside) newBrush)

markBrushSidesByNumbers ::
    Brush (WithSurfaceNumbers a) b ->
    DS.DisjointSet s ->
    ST s (Brush (WithBrushSides a) b)
markBrushSidesByNumbers brush dsetSurf = do
    outsideRoot <- DS.findSubsetRoot dsetSurf 0
    let findBrushSide i = do
            ir <- DS.findSubsetRoot dsetSurf i
            if ir == outsideRoot
                then pure Outside
                else pure Inside
    markedFaces <-
        RTree.traverseStable
            (\face -> do
                forM face $ \(WithSurfaceNumbers bi fi _ otherData) -> do
                    bside <- findBrushSide bi
                    fside <- findBrushSide fi
                    pure $ WithBrushSides bside fside otherData
            )
            (brushFaces brush)
    pure $
        brush
            { brushFaces = markedFaces
            }

data WithZoneNumbers a =
    WithZoneNumbers
        {-# UNPACK #-} !Int {- back side -}
        {-# UNPACK #-} !Int {- front side -}
        a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

Aeson.deriveJSON Aeson.defaultOptions ''WithZoneNumbers

instance (ToWolfMod a) => ToWolfMod (WithZoneNumbers a) where
    toWolfMod (WithZoneNumbers bs fs a) =
        toWolfMod a ++ [wfsides]
      where
        wfsides = wolfApp "FaceForm" $
            [ wolfList
                [ wolfApp "ColorData[1]" [toWolf fs]
                ]
            , wolfList
                [ wolfApp "ColorData[1]" [toWolf bs]
                ]
            ]

data WithZoneAtInfinity b =
    WithZoneAtInfinity
        {-# UNPACK #-} !Int
        b
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

Aeson.deriveJSON Aeson.defaultOptions ''WithZoneAtInfinity

instance (ToWolfMod a) => ToWolfMod (WithZoneAtInfinity a) where
    toWolfMod (WithZoneAtInfinity _ a) = toWolfMod a

markBrushZones ::
    (Show a) =>
    Brush a b ->
    Brush (WithZoneNumbers a) (WithZoneAtInfinity b)
markBrushZones originalBrush = do
    runST $ do
        (brushWithNumbers, dsetSurf) <- numberBrushSurfaces originalBrush
        markBrushZonesByNumbers brushWithNumbers dsetSurf

markBrushZonesByNumbers ::
    Brush (WithSurfaceNumbers a) b ->
    DS.DisjointSet s ->
    ST s (Brush (WithZoneNumbers a) (WithZoneAtInfinity b))
markBrushZonesByNumbers brush dsetSurf = do
    dsetSurfRoots <- DS.getDisjointSetRoots dsetSurf
    let zoneMap = makeZoneMap 0 dsetSurfRoots
    let findBrushZone i = do
            ir <- DS.findSubsetRoot dsetSurf i
            pure $! zoneMap Map.! ir
    markedFaces <-
        RTree.traverseStable
            (\face -> do
                forM face $ \(WithSurfaceNumbers bi fi _ otherData) -> do
                    bzone <- findBrushZone bi
                    fzone <- findBrushZone fi
                    pure (WithZoneNumbers bzone fzone otherData)
            )
            (brushFaces brush)
    zinf <- findBrushZone 0
    pure $
        Brush
            { brushFaces = markedFaces
            , brushData = WithZoneAtInfinity zinf (brushData brush)
            }
  where
    makeZoneMap _ [] = Map.empty
    makeZoneMap i (sroot : rest) =
        Map.insert sroot i $ makeZoneMap (i + 1) rest
