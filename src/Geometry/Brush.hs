module Geometry.Brush
    ( module Geometry.Brush.Type
    , module Geometry.Brush.Merging
    , module Geometry.Brush.Zoning
    , triangulateBrush
    )
where

import Data.Sequence (Seq (..))
import Data.Monoid
import Geometry.Brush.Type
import Geometry.Brush.Merging
import Geometry.Brush.Zoning
import Geometry.PlaneSpace
import Geometry.Triangle
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Geometry.Algorithm.Triangulate as Triangulate

-- qfaces :: [Face String]
-- qfaces =
    -- [ makeFace
        -- [Vec3 0 0 0, Vec3 10 0 0, Vec3 10 10 0, Vec3 0 10 0]
        -- "a"
    -- , makeFace
        -- [Vec3 5 5 0, Vec3 15 5 0, Vec3 15 15 0, Vec3 5 15 0]
        -- "b"
    -- ]
    -- [ makeFace
        -- [Vec3 1 1 5, Vec3 22 1 5, Vec3 21 5 5, Vec3 19 9 5, Vec3 1 8 5]
        -- ("mid" :: String)
    -- , makeFace [aaa, aba, bba, baa] "bottom"
    -- , makeFace [aab, bab, bbb, abb] "top"
    -- , makeFace [aaa, baa, bab, aab] "front"
    -- , makeFace [aba, abb, bbb, bba] "back"
    -- , makeFace [aaa, aab, abb, aba] "left"
    -- , makeFace [baa, bba, bbb, bab] "right"
    -- ]
  -- where
    -- aaa = Vec3  0  0  0
    -- aab = Vec3  0  0 10
    -- aba = Vec3  0 10  0
    -- abb = Vec3  0 10 10
    -- baa = Vec3 10  0  0
    -- bab = Vec3 10  0 10
    -- bba = Vec3 10 10  0
    -- bbb = Vec3 10 10 10

-- qbrush :: Brush (WithCuts String) String
-- qbrush = List.foldl' appendFace (emptyBrush "B") qfaces

-- qbrush2 :: Brush String String
-- qbrush2 = executeBrushFaceSplit qbrush

-- qbrush3 :: Brush (WithBrushSides String) (WithSideAtInfinity String)
-- qbrush3 = markBrushSides qbrush2

-- qworld :: Brush (WithBrushSides String) (WithSideAtInfinity String)
-- qworld = emptyBrush (WithSideAtInfinity Inside ("w" :: String))

-- qworld2 :: Brush (WithBrushSides String) (WithSideAtInfinity String)
-- qworld2 = brushOperSubtractSolid qworld qbrush3

-- qbrushEdgeMap :: Map.Map BrushEdge (NonEmpty (FaceSide, Face String))
-- qbrushEdgeMap =
    -- Map.mapWithKey sortEdgeConnectedFaces (constructEdgeMap qbrush2)

triangulateBrush ::
    forall a b m.
    (Ord a, Show a, Monad m) =>
    (Int -> Int -> PlaneSpace -> m ()) ->
    Brush a b ->
    m (Seq (Triangle a))
triangulateBrush onPlane brush = do
    let planeMaps = Triangulate.makeBrushPlaneMaps brush
    execStateT
        (getAp $
            Map.foldMapWithKey
                (processPlane (Map.size planeMaps))
                planeMaps
        )
        1
  where
    processPlane ::
        Int ->
        (PlaneSpace, a) ->
        Triangulate.PlaneMap ->
        Ap (StateT Int m) (Seq (Triangle a))
    processPlane planeMapCount (ps, fdata) planeMap = Ap $ do
        i <- get
        lift $ onPlane i planeMapCount ps
        let !result =
                foldMap
                    (processTriangle ps fdata)
                    (Triangulate.triangulatePlaneMap planeMap)
        put (i + 1)
        pure result
    processTriangle ps fdata (pv1, pv2, pv3) =
        Seq.singleton $
            makeTriangle
                (fromPlaneSpace ps pv1)
                (fromPlaneSpace ps pv2)
                (fromPlaneSpace ps pv3)
                fdata



newtype StateT s m a = StateT
    { runStateT :: s -> m (a, s)
    }
    deriving (Functor)

execStateT :: (Monad m) => StateT s m a -> s -> m a
execStateT mx s0 = fmap fst $ runStateT mx s0

instance (Monad m) => Applicative (StateT s m) where
    pure x = StateT $ \s -> pure (x, s)
    mf <*> mx =
        StateT $ \s1 ->
            runStateT mf s1 >>= \(f, s2) ->
                runStateT mx s2 >>= \(x, s3) ->
                    pure (f x, s3)

instance (Monad m) => Monad (StateT s m) where
    mx >>= sel =
        StateT $ \s1 ->
            runStateT mx s1 >>= \(x, s2) ->
                runStateT (sel x) s2

lift :: (Monad m) => m a -> StateT s m a
lift mx = StateT $ \s -> mx >>= \x -> pure (x, s)

get :: (Applicative m) => StateT s m s
get = StateT $ \s -> pure (s, s)

put :: (Applicative m) => s -> StateT s m ()
put s = StateT $ \_ -> pure ((), s)
