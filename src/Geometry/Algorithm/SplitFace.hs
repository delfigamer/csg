module Geometry.Algorithm.SplitFace
    ( splitFace
    )
where

import Control.Exception
import Data.CList
import Data.Either
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import Geometry.AngleRat
import Geometry.Class
import Geometry.Vec2
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Debug.Trace

-- fooContour =
    -- [ Vec2   128  (-128)
    -- , Vec2    48   (-48)
    -- , Vec2     0      0
    -- , Vec2 (-128)   128
    -- , Vec2 (-128) (-128)
    -- ]

-- fooCuts =
    -- [ line2Between (Vec2   32  (-64)) (Vec2   48  (-64))
    -- , line2Between (Vec2    0     0)  (Vec2 (-16)    0)
    -- , line2Between (Vec2 (-16)    0)  (Vec2 (-16) (-48))
    -- , line2Between (Vec2   48  (-64)) (Vec2   48  (-48))
    -- ]

-- fooStr = "base=" ++ baseStr fooContour fooCuts ++ ";\nGraphics[{AbsoluteThickness[3],base},Axes->True]"

-- foo = splitFace fooContour fooCuts



data HalfEdge = HalfEdge !Vec2 !Vec2 !HalfPlane
    deriving (Show, Eq, Ord)

halfEdge :: Vec2 -> Vec2 -> HalfEdge
halfEdge va vb =
    HalfEdge va vb (leftHalfPlane va vb)

splitFace :: [Vec2] -> [Line2] -> [[Vec2]]
splitFace originalFace originalCuts = do
    let faceEdges = mapPairsLoop halfEdge originalFace
    let cutEdges =
            concatMap
                (\(Line2 o d) ->
                    [ halfEdge o (o +. d)
                    , halfEdge (o +. d) o
                    ]
                )
                originalCuts
    let result = buildPatches $ faceEdges ++ cutEdges
    assert (all isNonDegenerate result) result
  where
    isNonDegenerate vs = length vs >= 3

buildPatches :: [HalfEdge] -> [[Vec2]]
buildPatches [] = []
buildPatches (HalfEdge a0 b0 hp0 : otherEdges) =
    buildPatchesPrepare a0 (b0 -. a0) hp0 otherEdges

buildPatchesPrepare ::
    Vec2 ->
    Vec2 ->
    HalfPlane ->
    [HalfEdge] ->
    [[Vec2]]
buildPatchesPrepare pivot xray yhplane = go Map.empty Map.empty []
  where
    go pendingEdges blockerWindow retiredEdges [] =
        buildPatchesSweep
            pivot
            xray
            yhplane
            yhplane
            (pivot +. xray :| [pivot])
            pendingEdges
            blockerWindow
            retiredEdges
    go pendingEdges blockerWindow retiredEdges (edge@(HalfEdge a b hp) : otherEdges) = do
        let ahd = homdot2 yhplane a
        let bhd = homdot2 yhplane b
        if
            | ahd > 0 ->
                go
                    (insertEdgeAt a edge pendingEdges)
                    blockerWindow
                    retiredEdges
                    otherEdges
            | ahd <= 0 && bhd > 0 && a /= pivot && homdot2 hp pivot >= 0 ->
                go
                    pendingEdges
                    (insertEdgeAt b edge blockerWindow)
                    retiredEdges
                    otherEdges
            | otherwise ->
                go
                    pendingEdges
                    blockerWindow
                    (edge : retiredEdges)
                    otherEdges
    insertEdgeAt pt edge =
        Map.insertWith (<>) (angleRat2 xray (pt -. pivot)) (edge :| [])

buildPatchesSweep ::
    Vec2 ->
    Vec2 ->
    HalfPlane ->
    HalfPlane ->
    NonEmpty Vec2 ->
    Map.Map AngleRat (NonEmpty HalfEdge) ->
    Map.Map AngleRat (NonEmpty HalfEdge) ->
    [HalfEdge] ->
    [[Vec2]]
buildPatchesSweep
        pivot xray
        yhplane lasthplane
        patchPoints@(lastPoint :| _) pendingEdges blockerWindow retiredEdges =
    trace "\n\nbuildPatchesSweep\n" $
    trace ("patchPoints=" ++ vec2ListStr (NonEmpty.toList patchPoints) ++ ";") $
    trace ("startEdge=" ++ halfEdgeStr (HalfEdge pivot (pivot +. xray) yhplane) ++ ";") $
    trace ("pendingEdges=" ++ halfEdgeListStr (concatMap NonEmpty.toList (Map.elems pendingEdges)) ++ ";") $
    trace ("blockerWindow=" ++ halfEdgeListStr (concatMap NonEmpty.toList (Map.elems blockerWindow)) ++ ";") $
    trace ("retiredEdges=" ++ halfEdgeListStr retiredEdges ++ ";") $
    trace ("> length retiredEdges = " ++ show (length retiredEdges)) $
        case Map.minViewWithKey pendingEdges of
            Just ((sweepAr, edgeNList), restPending)
                | sweepAr < AngleRatNegX -> trace "> sweep" $ do
                    let (retiredBlockers, reducedBlockerWindow) =
                            Map.spanAntitone (\ar -> ar <= sweepAr) blockerWindow
                    let retiredBlockerList =
                            concatMap NonEmpty.toList $ Map.elems retiredBlockers
                    let (skippedBlockers, newBlockers) =
                            partitionEithers $
                                map (edgeToBlocker pivot xray) (NonEmpty.toList edgeNList)
                    let newBlockerWindow =
                            foldr (\(ar, edge) -> Map.insertWith (<>) ar edge) reducedBlockerWindow newBlockers
                    let Arg _ newPoint =
                            minimum $
                                NonEmpty.map (\(HalfEdge a _ _) -> Arg (dotSqr (a -. pivot)) a) $
                                    edgeNList
                    if pointNotBlockedBy lasthplane reducedBlockerWindow newPoint
                        then trace "> point not blocked" $ do
                            let !newRetiredSet =
                                    case List.partition (\(HalfEdge a b _) -> a == lastPoint && b == newPoint) retiredBlockerList of
                                        ([], _) -> trace "> cut" $ do
                                            [halfEdge newPoint lastPoint] ++ retiredBlockerList ++ skippedBlockers ++ retiredEdges
                                        ([_], filteredRetiredBlockerList) -> trace "> consume" $
                                            filteredRetiredBlockerList ++ skippedBlockers ++ retiredEdges
                                        _ -> error "bad topology"
                            buildPatchesSweep
                                pivot
                                xray
                                yhplane
                                (leftHalfPlane lastPoint newPoint)
                                (newPoint NonEmpty.<| patchPoints)
                                restPending
                                newBlockerWindow
                                newRetiredSet
                        else trace "> point blocked" $ do
                            buildPatchesSweep
                                pivot
                                xray
                                yhplane
                                lasthplane
                                patchPoints
                                restPending
                                newBlockerWindow
                                (retiredBlockerList ++ skippedBlockers ++ retiredEdges)
            _ -> trace "> stop" $ do
                let pendingList =
                        concatMap NonEmpty.toList $ Map.elems pendingEdges
                let retiredBlockerList =
                        concatMap NonEmpty.toList $ Map.elems blockerWindow
                let newWorkset =
                        case List.partition (\(HalfEdge a b _) -> a == lastPoint && b == pivot) retiredBlockerList of
                            ([], _) -> trace "> cut" $ do
                                [halfEdge pivot lastPoint] ++ pendingList ++ retiredBlockerList ++ retiredEdges
                            ([_], filteredRetiredBlockerList) -> trace "> consume" $ do
                                pendingList ++ filteredRetiredBlockerList ++ retiredEdges
                            _ -> error "bad topology"
                trace ("newWorkset=" ++ halfEdgeListStr newWorkset ++ ";") $
                    trace ("patch=" ++ vec2ListStr (pivot : NonEmpty.toList patchPoints) ++ ";") $
                        reverse (NonEmpty.toList patchPoints) : buildPatches newWorkset

edgeToBlocker ::
    Vec2 -> Vec2 -> HalfEdge ->
    Either HalfEdge (AngleRat, NonEmpty HalfEdge)
edgeToBlocker pivot xray edge@(HalfEdge _ b hp)
    | b == pivot = Right (AngleRatNegX, edge :| [])
    | hdp > 0 = Right (angleRat2 xray (b -. pivot), edge :| [])
    | otherwise = Left edge
  where
    hdp = homdot2 hp pivot

pointNotBlockedBy ::
    HalfPlane ->
    Map.Map AngleRat (NonEmpty HalfEdge) ->
    Vec2 ->
    Bool
pointNotBlockedBy lasthplane blockerWindow point =
    all (\hp -> homdot2 hp point >= 0) (lasthplane : blockerhplanes)
  where
    blockerhplanes =
        map (\(HalfEdge _ _ hp) -> hp) $
        concatMap NonEmpty.toList $
        Map.elems blockerWindow







{- debug -}

trace :: String -> a -> a
trace x =
    if False
        then Debug.Trace.trace x
        else id

-- runSplitTest :: IO ()
-- runSplitTest = do
    -- let patches = splitFace (map mapTo testFace) (map (both mapTo) testCuts)
    -- let patchEdges = concatMap (mapPairsLoop (halfEdge (Vec3 0 0 1)) . map mapFrom) patches
    -- putStrLn $ "resultEdges=" ++ halfEdgeListStr patchEdges ++ ";"
  -- where
    -- both f (a, b) = (f a, f b)
    -- mapTo (Vec3 x y z) = Vec3 x (- y - z) (y - z)
    -- mapFrom (Vec3 x y z) = Vec3 x (- 0.5 * y + 0.5 * z) (- 0.5 * y - 0.5 * z)

-- runBuildPatchesTest :: IO ()
-- runBuildPatchesTest = do
    -- forM_ [0 .. length testEdges - 1] $ \i -> do
        -- putStrLn $ "\n" ++ show i ++ "\n"
        -- e <- try $ evaluate $ buildPatches $ rotateList i testEdges
        -- case e of
            -- Left (SomeException _) -> pure ()
            -- Right _ -> pure ()

-- rotateList :: Int -> [a] -> [a]
-- rotateList n xs = do
    -- let nn = n `mod` length xs
    -- drop nn xs ++ take nn xs

-- testEdges :: [HalfEdge]
-- testEdges = [
    -- HalfEdge (Vec2  20   0) (Vec2  50   0) (HalfPlane   0    1      0),
    -- HalfEdge (Vec2  50   0) (Vec2 100   0) (HalfPlane   0    1      0),
    -- HalfEdge (Vec2 100   0) (Vec2 140  60) (HalfPlane (-3)   2    300),
    -- HalfEdge (Vec2 140  60) (Vec2  90 100) (HalfPlane (-4) (-5)   860),
    -- HalfEdge (Vec2  90 100) (Vec2  50  20) (HalfPlane   2  (-1)  (-80)),
    -- HalfEdge (Vec2  50  20) (Vec2  45  30) (HalfPlane (-2) (-1)   120),
    -- HalfEdge (Vec2  45  30) (Vec2  20  80) (HalfPlane (-2) (-1)   120),
    -- HalfEdge (Vec2  20  80) (Vec2   0  40) (HalfPlane   2  (-1)    40),
    -- HalfEdge (Vec2   0  40) (Vec2  20   0) (HalfPlane   2    1   (-40)),
    -- HalfEdge (Vec2  50   0) (Vec2  45  10) (HalfPlane (-2) (-1)   100),
    -- HalfEdge (Vec2  45  10) (Vec2  50   0) (HalfPlane   2    1  (-100)),
    -- HalfEdge (Vec2  45  10) (Vec2  35  10) (HalfPlane   0  (-1)    10),
    -- HalfEdge (Vec2  35  10) (Vec2  45  10) (HalfPlane   0    1   (-10)),
    -- HalfEdge (Vec2  35  10) (Vec2  45  30) (HalfPlane (-2)   1     60),
    -- HalfEdge (Vec2  45  30) (Vec2  35  10) (HalfPlane   2  (-1)  (-60)),
    -- HalfEdge (Vec2  85  40) (Vec2  80  60) (HalfPlane (-4) (-1)   380),
    -- HalfEdge (Vec2  80  60) (Vec2  85  40) (HalfPlane   4    1  (-380)),
    -- HalfEdge (Vec2  80  60) (Vec2 100  60) (HalfPlane   0    1   (-60)),
    -- HalfEdge (Vec2 100  60) (Vec2  80  60) (HalfPlane   0  (-1)    60),
    -- HalfEdge (Vec2 100  60) (Vec2 105  35) (HalfPlane   5    1  (-560)),
    -- HalfEdge (Vec2 105  35) (Vec2 100  60) (HalfPlane (-5) (-1)   560),
    -- HalfEdge (Vec2 105  35) (Vec2  85  40) (HalfPlane (-1) (-4)   245),
    -- HalfEdge (Vec2  85  40) (Vec2 105  35) (HalfPlane   1    4  (-245)),
    -- HalfEdge (Vec2 120  60) (Vec2 100  80) (HalfPlane (-1) (-1)   180),
    -- HalfEdge (Vec2 100  80) (Vec2 120  60) (HalfPlane   1    1  (-180))]

-- testVecs :: [Vec2]
-- testVecs =
    -- [Vec2 0 0, Vec2 1 0, Vec2 1 1, Vec2 0 1, Vec2 (-1) 1, Vec2 (-1) 0, Vec2 (-1) (-1), Vec2 0 (-1), Vec2 1 (-1)]

-- testFace :: [Vec3]
-- testFace = rotateList 0 $
    -- [ Vec3  20   0   0
    -- , Vec3  50   0   0
    -- , Vec3 100   0   0
    -- , Vec3 140  60   0
    -- , Vec3  90 100   0
    -- , Vec3  50  20   0
    -- , Vec3  45  30   0
    -- , Vec3  20  80   0
    -- , Vec3   0  40   0
    -- ]

-- testCuts :: [(Vec3, Vec3)]
-- testCuts =
    -- [ (Vec3  50   0   0, Vec3  45  10   0)
    -- , (Vec3  45  10   0, Vec3  35  10   0)
    -- , (Vec3  35  10   0, Vec3  45  30   0)
    -- , (Vec3  85  40   0, Vec3  80  60   0)
    -- , (Vec3  80  60   0, Vec3 100  60   0)
    -- , (Vec3 100  60   0, Vec3 105  35   0)
    -- , (Vec3 105  35   0, Vec3  85  40   0)
    -- , (Vec3 120  60   0, Vec3 100  80   0)
    -- ]

-- baseStr :: [Vec2] -> [Line2] -> String
-- baseStr contour cuts =
    -- "{"
    -- ++ vec2ListStr (last contour : contour)
    -- ++ concatMap (\(Line2 a d) -> "," ++ vec2ListStr [a, a +. d]) cuts
    -- ++ "}"

vec2ListStr :: [Vec2] -> String
vec2ListStr vs =
    "Line[{" ++ List.intercalate "," (map vec2Str vs) ++ "}]"
  where
    vec2Str (Vec2 x y) =
        "{" ++ show (fromRational x :: Double) ++ "," ++ show (fromRational y :: Double) ++ "}"

-- halfPlaneStr :: Vec3 -> HalfSpace -> String
-- halfPlaneStr vec@(Vec3 vxr vyr _) hp@(HalfSpace (IVec3 pxi pyi _) _)
    -- | homdot3 hp vec /= 0 = error "bad topology"
    -- | otherwise =
        -- "Polygon[{"
        -- ++ "{" ++ show ax ++ "," ++ show ay ++ "},"
        -- ++ "{" ++ show bx ++ "," ++ show by ++ "},"
        -- ++ "{" ++ show (bx - st*tx - sn*ty) ++ "," ++ show (by - st*ty + sn*tx) ++ "},"
        -- ++ "{" ++ show (ax + st*tx - sn*ty) ++ "," ++ show (ay + st*ty + sn*tx) ++ "},"
        -- ++ "{" ++ show ax ++ "," ++ show ay ++ "}}]"
  -- where
    -- st = 1.5
    -- sn = 2
    -- ax, ay, bx, by, dx, dy, tx, ty :: Double
    -- ax = fromRational vxr
    -- ay = fromRational vyr
    -- bx = ax + 100 * tx
    -- by = ay + 100 * ty
    -- dx = fromInteger pyi
    -- dy = - fromInteger pxi
    -- tx = dx / sqrt (dx*dx + dy*dy)
    -- ty = dy / sqrt (dx*dx + dy*dy)

halfEdgeStr :: HalfEdge -> String
halfEdgeStr (HalfEdge (Vec2 axr ayr) (Vec2 bxr byr) _) =
    "Polygon[{"
    ++ "{" ++ show ax ++ "," ++ show ay ++ "},"
    ++ "{" ++ show bx ++ "," ++ show by ++ "},"
    ++ "{" ++ show (bx - st*tx - sn*ty) ++ "," ++ show (by - st*ty + sn*tx) ++ "},"
    ++ "{" ++ show (ax + st*tx - sn*ty) ++ "," ++ show (ay + st*ty + sn*tx) ++ "},"
    ++ "{" ++ show ax ++ "," ++ show ay ++ "}}]"
  where
    st = 3
    sn = 4
    ax, ay, bx, by, dx, dy, tx, ty :: Double
    ax = fromRational axr
    ay = fromRational ayr
    bx = fromRational bxr
    by = fromRational byr
    dx = bx - ax
    dy = by - ay
    tx = dx / sqrt (dx*dx + dy*dy)
    ty = dy / sqrt (dx*dx + dy*dy)

halfEdgeListStr :: [HalfEdge] -> String
halfEdgeListStr hs = "{" ++ List.intercalate "," (map halfEdgeStr hs) ++ "}"
