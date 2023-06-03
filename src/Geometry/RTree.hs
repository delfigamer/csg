{-# LANGUAGE AllowAmbiguousTypes #-}

module Geometry.RTree
    ( RTreeParams (..)
    , KnownRTreeParams (..)
    , DefaultRTreeParams
    , RTreeT
    , RTree
    , map
    , mapStable
    , traverse
    , traverseStable
    , treeMerge
    , empty
    , singleton
    , append
    , fromList
    , traverseIntersecting
    , foldStructured
    , foldMapIntersecting
    , foldrIntersecting
    , concatTraverseIntersecting
    , concatMapIntersecting
    , filterCoarseIntersecting
    , concatTraverseIntersectingTree
    , concatMapIntersectingTree
    , xAxisRaycast
    )
where

import Prelude hiding (map, traverse)
import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Function
import Data.Functor.Const
import Data.Functor.Identity
import Data.Interval
import Data.Peano
import Data.Proxy
import Data.Semigroup
import GHC.TypeLits
import Geometry.BBox
import Geometry.Class
import Geometry.Vec3
import qualified Data.Aeson as Aeson
import qualified Data.ShortVector as SV
import qualified Data.List as List
import qualified Data.Traversable as Traversable

data RTreeParams = RTreeParams Nat Nat Nat

class KnownRTreeParams (n :: RTreeParams) where
    minimumPop :: Int
    maximumPop :: Int
    leftoverPop :: Int

instance
    (KnownNat a, KnownNat b, KnownNat c) =>
    KnownRTreeParams ('RTreeParams a b c)
  where
    minimumPop = fromInteger $ natVal $ Proxy @a
    maximumPop = fromInteger $ natVal $ Proxy @b
    leftoverPop = fromInteger $ natVal $ Proxy @c

type DefaultRTreeParams = 'RTreeParams 4 8 6

data family NodeData (n :: RTreeParams) (p :: P) :: * -> *

newtype instance NodeData n 'Z a = Leaf
    { unLeaf :: a
    }

newtype instance NodeData n ('S p) a = Branch
    { unBranch :: SV.ShortVector (Node n p a)
    }

data Node n p a = Node BBox (NodeData n p a)

instance (KnownP p, Show a) => Show (Node n p a) where
    showsPrec di ndata = showsPrecNode di knownPI ndata

showsPrecNodeList :: (Show a) => PI p -> [Node n p a] -> ShowS
showsPrecNodeList _ [] =
    showString "[]"
showsPrecNodeList p (x0 : xs0) =
    showString "[" .
    showsPrecNode 0 p x0 .
    go xs0 .
    showString "]"
  where
    go (x : xs) =
        showString "," .
        showsPrecNode 0 p x .
        go xs
    go [] =
        id

showsPrecNode :: (Show a) => Int -> PI p -> Node n p a -> ShowS
showsPrecNode di p (Node _ ndata) =
    case p of
        ZI ->
            case ndata of
                Leaf x ->
                    showParen (di > 10) $
                        showString "nodeLeaf " .
                        showsPrec 11 x
        SI np ->
            case ndata of
                Branch br ->
                    showParen (di > 10) $
                        showString "nodeBranch " .
                        showsPrecNodeList np (toList br)

instance HasBBox (Node n p a) where
    bboxOf (Node bbox _) = bbox

data OneOrTwo a
    = One a
    | Two a a
    deriving (Show)

nodeLeaf :: (HasBBox a) => a -> Node n 'Z a
nodeLeaf a =
    Node (bboxOf a) (Leaf a)

nodeBranch :: SV.ShortVector (Node n p a) -> Node n ('S p) a
nodeBranch nodes =
    Node (foldableBBox nodes) (Branch nodes)

buildSplitting ::
    forall n p a.
    (KnownRTreeParams n) =>
    SV.ShortVector (Node n p a) ->
    OneOrTwo (Node n ('S p) a)
buildSplitting nodes
    | SV.length nodes > 2 * maximumPop @n =
        error "too many nodes"
    | SV.length nodes < maximumPop @n =
        One $ nodeBranch nodes
    | otherwise =
        case chosenSplit of
            (n1, n2) ->
                Two n1 n2
  where
    xsplits = makeSplitList $ SV.sortOn (bboxXInterval . bboxOf) nodes
    ysplits = makeSplitList $ SV.sortOn (bboxYInterval . bboxOf) nodes
    zsplits = makeSplitList $ SV.sortOn (bboxZInterval . bboxOf) nodes
    chosenSplitList =
        minimumBy
            (compare `on` totalMargin)
            [xsplits, ysplits, zsplits]
    chosenSplit =
        minimumBy
            (compare `on` \x -> (splitOverlap x, splitMargin x))
            chosenSplitList
    totalMargin slist =
        getSum $ foldMap (Sum . splitMargin) slist
    splitMargin (n1, n2) =
        bboxArea (bboxOf n1) + bboxArea (bboxOf n2)
    splitOverlap (n1, n2) =
        bboxVolume $ bboxOf n1 `intersect` bboxOf n2
    makeSplitList nvec = do
        let imposedMinimumPop =
                max
                    (minimumPop @n)
                    (SV.length nvec - maximumPop @n)
        at <- [imposedMinimumPop .. SV.length nvec - imposedMinimumPop]
        [(nodeBranch $ SV.take at nvec, nodeBranch $ SV.drop at nvec)]

buildOverflowing ::
    forall n p a.
    (KnownRTreeParams n) =>
    SV.ShortVector (Node n p a) ->
    (Node n ('S p) a, SV.ShortVector (Node n p a))
buildOverflowing nodes
    | SV.length nodes < maximumPop @n =
        ( nodeBranch nodes
        , SV.empty
        )
    | otherwise =
        ( nodeBranch (SV.take (leftoverPop @n) nodesSorted)
        , SV.drop (leftoverPop @n) nodesSorted
        )
  where
    directoryBBox = foldableBBox nodes
    directoryCenter = bboxDoubleCenter directoryBBox
    nodesSorted = SV.sortOn nodeDistance nodes
    nodeDistance n =
        dotSqr (bboxDoubleCenter (bboxOf n) -. directoryCenter)

chooseSubnodeByOverlap ::
    SV.ShortVector BBox ->
    BBox ->
    Int
chooseSubnodeByOverlap givenBBoxVec newBBox =
    minimumBy
        (compare `on` \i ->
            (insertionOverlap i, insertionEnlargement i, insertionArea i)
        )
        [0 .. length givenBBoxVec - 1]
  where
    insertionOverlap i =
        bboxVecOverlap $
            SV.modify
                i
                (hull newBBox)
                givenBBoxVec
    insertionEnlargement i =
        bboxArea ((givenBBoxVec SV.!! i) `hull` newBBox) -
        insertionArea i
    insertionArea i =
        bboxArea $ (givenBBoxVec SV.!! i)
    bboxVecOverlap v =
        getSum $
            SV.foldMapWithIndex'
                (\i1 b1 ->
                    foldMap'
                        (\i2 ->
                            Sum $ bboxVolume (b1 `intersect` (v SV.!! i2))
                        )
                        [i1 + 1 .. length v - 1]
                )
                v

chooseSubnodeByArea ::
    SV.ShortVector BBox ->
    BBox ->
    Int
chooseSubnodeByArea givenBBoxVec newBBox =
    minimumBy
        (compare `on` \i ->
            (insertionEnlargement i, insertionArea i)
        )
        [0 .. length givenBBoxVec - 1]
  where
    insertionEnlargement i =
        bboxArea ((givenBBoxVec SV.!! i) `hull` newBBox) -
        insertionArea i
    insertionArea i =
        bboxArea $ (givenBBoxVec SV.!! i)

chooseSubnode ::
    PI p ->
    SV.ShortVector (Node n p a) ->
    BBox ->
    Int
chooseSubnode (SI ZI) nodeVec = chooseSubnodeByOverlap (fmap bboxOf nodeVec)
chooseSubnode _ nodeVec = chooseSubnodeByArea (fmap bboxOf nodeVec)

insertIntoVecSplitting ::
    (KnownRTreeParams n) =>
    PI p ->
    SV.ShortVector (Node n ('S p) a) ->
    Node n p a ->
    SV.ShortVector (Node n ('S p) a)
insertIntoVecSplitting p nodeVec newNode =
    case buildSplitting (SV.append subnodeBr newNode) of
        One newSubnode ->
            SV.set subnodeIndex newSubnode nodeVec
        Two newPartA newPartB ->
            SV.append (SV.set subnodeIndex newPartA nodeVec) newPartB
  where
    subnodeIndex = chooseSubnode (SI p) nodeVec (bboxOf newNode)
    Node _ (Branch subnodeBr) = nodeVec SV.!! subnodeIndex

nodeMergeAsymmetric ::
    (KnownRTreeParams n) =>
    PI pd ->
    Node n ('S (Add pd p2)) a ->
    PI p2 ->
    Node n p2 a ->
    SV.ShortVector (Node n (Add pd p2) a)
nodeMergeAsymmetric ZI (Node _ (Branch nodeVec)) _ newNode =
    SV.append nodeVec newNode
nodeMergeAsymmetric pd@(SI npd) (Node _ (Branch nodeVec)) pb newNode = do
    let subnodeIndex = chooseSubnode (addPI pd pb) nodeVec (bboxOf newNode)
    let oldSubnode = nodeVec SV.!! subnodeIndex
    let newSubnodeChildren = nodeMergeAsymmetric npd oldSubnode pb newNode
    case buildOverflowing newSubnodeChildren of
        (newSubnode, overflow) ->
            foldl'
                (\vec n -> insertIntoVecSplitting (addPI npd pb) vec n)
                (SV.set subnodeIndex newSubnode nodeVec)
                overflow

nodeMergeSymmetric ::
    PI p ->
    Node n ('S p) a ->
    Node n ('S p) a ->
    SV.ShortVector (Node n p a)
nodeMergeSymmetric _ (Node _ (Branch nodeVecA)) (Node _ (Branch nodeVecB)) =
    nodeVecA <> nodeVecB

data RTreeT n a
    = forall p. FullT (PI p) (Node n p a)
    | EmptyT

type RTree = RTreeT DefaultRTreeParams

treeMerge ::
    (KnownRTreeParams n) =>
    RTreeT n a ->
    RTreeT n a ->
    RTreeT n a
treeMerge EmptyT b =
    b
treeMerge b EmptyT =
    b
treeMerge (FullT pa na) (FullT pb nb) =
    case comparePI pa pb of
        LTPI pd ->
            mkTree pb $ nodeMergeAsymmetric pd nb pa na
        EQPI ->
            case pa of
                ZI ->
                    FullT (SI ZI) (node2 na nb)
                SI npa ->
                    mkTree pa $ nodeMergeSymmetric npa na nb
        GTPI pd ->
            mkTree pa $ nodeMergeAsymmetric pd na pb nb
  where
    node2 ::
        Node n p a ->
        Node n p a ->
        Node n ('S p) a
    node2 a b = nodeBranch $ SV.fromListN 2 [a, b]
    mkTree ::
        (KnownRTreeParams n) =>
        PI ('S p) ->
        SV.ShortVector (Node n p a) ->
        RTreeT n a
    mkTree p nvec =
        case buildSplitting nvec of
            One nr ->
                FullT p nr
            Two nr ns ->
                FullT (SI p) (node2 nr ns)

instance (Show a) => Show (RTreeT n a) where
    showsPrec _ EmptyT =
        showString "EmptyT"
    showsPrec di (FullT p0 n0) =
        showParen (di > 10) $
            showString "FullT " .
            showsPrecNode 11 p0 n0

instance (KnownRTreeParams n) => Semigroup (RTreeT n a) where
    (<>) = treeMerge

instance (KnownRTreeParams n) => Monoid (RTreeT n a) where
    mempty = EmptyT

instance Foldable (RTreeT n) where
    foldMap = go
      where
        go :: forall a m. (Monoid m) => (a -> m) -> RTreeT n a -> m
        go = coerce $ traverseStable @(Const m)
    null EmptyT = True
    null _ = False

map :: (HasBBox b) => (a -> b) -> RTreeT n a -> RTreeT n b
map = coerce $ traverse @Identity

mapStable :: (a -> b) -> RTreeT n a -> RTreeT n b
mapStable = coerce $ traverseStable @Identity

traverse ::
    (Applicative f, HasBBox b) =>
    (a -> f b) ->
    RTreeT n a ->
    f (RTreeT n b)
traverse _ EmptyT =
    pure EmptyT
traverse fn (FullT p0 n0) =
    FullT p0 <$> traverseNode fn p0 n0

traverseNode ::
    (Applicative f, HasBBox b) =>
    (a -> f b) ->
    PI p ->
    Node n p a ->
    f (Node n p b)
traverseNode fn p (Node _ ndata) =
    case p of
        ZI ->
            nodeLeaf <$> fn (unLeaf ndata)
        SI np ->
            nodeBranch <$>
                Traversable.traverse (traverseStableNode fn np) (unBranch ndata)

traverseStable ::
    (Applicative f) =>
    (a -> f b) ->
    RTreeT n a ->
    f (RTreeT n b)
traverseStable _ EmptyT =
    pure EmptyT
traverseStable fn (FullT p0 n0) =
    FullT p0 <$> traverseStableNode fn p0 n0

traverseStableNode ::
    (Applicative f) =>
    (a -> f b) ->
    PI p ->
    Node n p a ->
    f (Node n p b)
traverseStableNode fn p (Node nodeBBox ndata) =
    case p of
        ZI ->
            Node nodeBBox . Leaf <$>
                fn (unLeaf ndata)
        SI np ->
            Node nodeBBox . Branch <$>
                Traversable.traverse (traverseStableNode fn np) (unBranch ndata)

empty :: RTreeT n a
empty = EmptyT

singleton :: (HasBBox a) => a -> RTreeT n a
singleton x = FullT ZI (nodeLeaf x)

append :: (HasBBox a, KnownRTreeParams n) => RTreeT n a -> a -> RTreeT n a
append tree x = singleton x <> tree

fromList ::
    (Foldable t, HasBBox a, KnownRTreeParams n) =>
    t a ->
    RTreeT n a
fromList = foldl' append empty

traverseIntersecting ::
    (Applicative f, HasBBox a) =>
    (BBox -> Bool) ->
    (a -> f a) ->
    RTreeT n a ->
    f (RTreeT n a)
traverseIntersecting _ _ EmptyT =
    pure EmptyT
traverseIntersecting bboxTest fn (FullT p node) =
    FullT p <$> traverseIntersectingNode bboxTest fn p node

traverseIntersectingNode ::
    (HasBBox a, Applicative f) =>
    (BBox -> Bool) ->
    (a -> f a) ->
    PI p ->
    Node n p a ->
    f (Node n p a)
traverseIntersectingNode bboxTest fn p node@(Node nodeBBox ndata)
    | bboxTest nodeBBox =
        case p of
            ZI ->
                nodeLeaf <$>
                    fn (unLeaf ndata)
            SI np ->
                nodeBranch <$>
                    Traversable.traverse
                        (traverseIntersectingNode bboxTest fn np)
                        (unBranch ndata)
    | otherwise =
        pure node

foldStructured ::
    (a -> m) ->
    (Int -> BBox -> SV.ShortVector m -> m) ->
    RTreeT n a ->
    Maybe m
foldStructured _ _ EmptyT =
    Nothing
foldStructured fnLeaf fnBranch (FullT p node) =
    Just $ foldStructuredNode fnLeaf fnBranch p node

foldStructuredNode ::
    (a -> m) ->
    (Int -> BBox -> SV.ShortVector m -> m) ->
    PI p ->
    Node n p a ->
    m
foldStructuredNode fnLeaf fnBranch p (Node nodeBBox ndata) =
    case p of
        ZI ->
            fnLeaf (unLeaf ndata)
        SI np ->
            fnBranch
                (piToInt p)
                nodeBBox
                (fmap
                    (foldStructuredNode fnLeaf fnBranch np)
                    (unBranch ndata)
                )

foldMapIntersecting ::
    (Monoid m) =>
    (BBox -> Bool) ->
    (a -> m) ->
    RTreeT n a ->
    m
foldMapIntersecting _ _ EmptyT =
    mempty
foldMapIntersecting bboxTest fn (FullT p node) =
    foldMapIntersectingNode bboxTest fn p node

foldMapIntersectingNode ::
    (Monoid m) =>
    (BBox -> Bool) ->
    (a -> m) ->
    PI p ->
    Node n p a ->
    m
foldMapIntersectingNode bboxTest fn p (Node nodeBBox ndata)
    | bboxTest nodeBBox =
        case p of
            ZI ->
                fn (unLeaf ndata)
            SI np ->
                foldMap
                    (foldMapIntersectingNode bboxTest fn np)
                    (unBranch ndata)
    | otherwise =
        mempty

foldrIntersecting ::
    (BBox -> Bool) ->
    (a -> r -> r) ->
    r ->
    RTreeT n a ->
    r
foldrIntersecting bboxTest fn seed t =
    appEndo (foldMapIntersecting bboxTest (Endo . fn) t) seed

concatTraverseIntersecting ::
    (Applicative f, KnownRTreeParams n) =>
    (BBox -> Bool) ->
    (a -> f (RTreeT n a)) ->
    RTreeT n a ->
    f (RTreeT n a)
concatTraverseIntersecting _ _ EmptyT =
    pure EmptyT
concatTraverseIntersecting bboxTest fn (FullT p node) =
    concatTraverseIntersectingNode bboxTest fn p node

concatTraverseIntersectingNode ::
    (Applicative f, KnownRTreeParams n) =>
    (BBox -> Bool) ->
    (a -> f (RTreeT n a)) ->
    PI p ->
    Node n p a ->
    f (RTreeT n a)
concatTraverseIntersectingNode bboxTest fn p node@(Node nodeBBox ndata)
    | bboxTest nodeBBox =
        case p of
            ZI ->
                fn (unLeaf ndata)
            SI np ->
                fmap fold $
                    Traversable.traverse
                        (concatTraverseIntersectingNode bboxTest fn np)
                        (unBranch ndata)
    | otherwise =
        pure (FullT p node)

concatMapIntersecting ::
    (KnownRTreeParams n) =>
    (BBox -> Bool) ->
    (a -> RTreeT n a) ->
    RTreeT n a ->
    RTreeT n a
concatMapIntersecting =
    coerce (concatTraverseIntersecting @Identity)

filterCoarseIntersecting ::
    (BBox -> Bool) ->
    RTreeT n a ->
    RTreeT n a
filterCoarseIntersecting _ EmptyT =
    EmptyT
filterCoarseIntersecting bboxTest (FullT p node)
    | bboxTest (bboxOf node) =
        filterCoarseIntersectingNode bboxTest p node
    | otherwise =
        EmptyT

{- assumes that bboxTest (bboxOf node) == True -}
filterCoarseIntersectingNode ::
    (BBox -> Bool) ->
    PI p ->
    Node n p a ->
    RTreeT n a
filterCoarseIntersectingNode bboxTest p node@(Node _ ndata) =
    case p of
        ZI ->
            FullT p node
        SI np -> do
            let interiorSubnodes =
                    List.filter (bboxTest . bboxOf) $
                    toList $ unBranch ndata
            case interiorSubnodes of
                [] ->
                    EmptyT
                [a] ->
                    filterCoarseIntersectingNode bboxTest np a
                _ ->
                    FullT p (nodeBranch $ SV.fromList interiorSubnodes)

concatTraverseIntersectingTree ::
    (Applicative f, KnownRTreeParams n) =>
    (RTreeT n a -> BBox -> f (RTreeT n c)) ->
    (a -> RTreeT n b -> f (RTreeT n c)) ->
    RTreeT n a ->
    RTreeT n b ->
    f (RTreeT n c)
concatTraverseIntersectingTree _ _ EmptyT _ =
    pure EmptyT
concatTraverseIntersectingTree onClear onIntersect (FullT p node) btree =
    concatTraverseIntersectingTreeNode onClear onIntersect p node btree

concatTraverseIntersectingTreeNode ::
    (Applicative f, KnownRTreeParams n) =>
    (RTreeT n a -> BBox -> f (RTreeT n c)) ->
    (a -> RTreeT n b -> f (RTreeT n c)) ->
    PI p ->
    Node n p a ->
    RTreeT n b ->
    f (RTreeT n c)
concatTraverseIntersectingTreeNode onClear onIntersect p node@(Node nodeBBox ndata) btree
    | null filteredBTree =
        onClear (FullT p node) nodeBBox
    | otherwise =
        case p of
            ZI ->
                onIntersect (unLeaf ndata) btree
            SI np ->
                fmap fold $
                    Traversable.traverse
                        (\s ->
                            concatTraverseIntersectingTreeNode
                                onClear onIntersect np s filteredBTree
                        )
                        (unBranch ndata)
  where
    filteredBTree = filterCoarseIntersecting (overlaps nodeBBox) btree

concatMapIntersectingTree ::
    (KnownRTreeParams n) =>
    (RTreeT n a -> BBox -> RTreeT n c) ->
    (a -> RTreeT n b -> RTreeT n c) ->
    RTreeT n a ->
    RTreeT n b ->
    RTreeT n c
concatMapIntersectingTree =
    coerce $ concatTraverseIntersectingTree @Identity

xAxisRaycast ::
    (Semigroup r) =>
    Vec3 ->
    (a -> Maybe (Rational, r)) ->
    RTreeT n a ->
    Maybe r
xAxisRaycast (Vec3 vx vy vz) fn rtree = do
    case rtree of
        FullT p node ->
            fmap snd $
                xAxisRaycastNode (XRay3 vy vz) vx fn p node Nothing
        EmptyT ->
            Nothing

xAxisRaycastNode ::
    (Semigroup r) =>
    XRay3 ->
    Rational ->
    (a -> Maybe (Rational, r)) ->
    PI p ->
    Node n p a ->
    Maybe (Rational, r) ->
    Maybe (Rational, r)
xAxisRaycastNode xray xmax fn p (Node _ ndata) = do
    case p of
        ZI -> \mbOldState -> do
            case fn (unLeaf ndata) of
                mbNewState@(Just (newX, newResult))
                    | newX < xmax ->
                        case mbOldState of
                            Nothing -> mbNewState
                            Just (oldX, oldResult)
                                | oldX < newX ->
                                    mbNewState
                                | oldX > newX ->
                                    mbOldState
                                | otherwise ->
                                    Just (oldX, oldResult <> newResult)
                _ -> mbOldState
        SI np -> do
            let filteredSubnodes = do
                    sn <- toList $ unBranch ndata
                    let snbox = bboxOf sn
                    guard $ bboxContainsXRay3 xray snbox
                    let Interval x1 x2 = bboxXInterval snbox
                    guard $ x1 < xmax
                    [(x2, sn)]
            go (insertionSortDesc filteredSubnodes)
          where
            go [] mbOldState = do
                mbOldState
            go ((x2, subnode) : otherSubnodes) mbOldState = do
                let mbNewState =
                        xAxisRaycastNode xray xmax fn np subnode mbOldState
                case mbOldState of
                    Just (oldX, _)
                        | oldX > x2 ->
                            mbOldState
                    _ -> do
                        go otherSubnodes mbNewState

insertionSortDesc ::
    (Ord k) =>
    [(k, v)] ->
    [(k, v)]
insertionSortDesc = foldr insertOrderedDesc []

insertOrderedDesc ::
    (Ord k) =>
    (k, v) ->
    [(k, v)] ->
    [(k, v)]
insertOrderedDesc kv@(k, _) = go
  where
    go list
        | h@(k2, _) : rest <- list
        , k < k2 =
            h : go rest
        | otherwise =
            kv : list

bboxArea :: BBox -> Rational
bboxArea (BBox (Interval ax bx) (Interval ay by) (Interval az bz))
    | ax <= bx && ay <= by && az <= bz = do
        let x = bx - ax
        let y = by - ay
        let z = bz - az
        x*(y + z) + y*z
    | otherwise = 0

bboxVolume :: BBox -> Rational
bboxVolume (BBox (Interval ax bx) (Interval ay by) (Interval az bz))
    | ax <= bx && ay <= by && az <= bz = do
        let x = bx - ax
        let y = by - ay
        let z = bz - az
        x*y*z
    | otherwise = 0

instance (Aeson.ToJSON a) => Aeson.ToJSON (RTreeT n a) where
    toJSON EmptyT = Aeson.Null
    toJSON (FullT p n) =
        Aeson.object
            [ "depth" Aeson..= piToInt p
            , "tree" Aeson..= withKnownP (Aeson.toJSON n) p
            ]

instance (Aeson.ToJSON a, KnownP p) => Aeson.ToJSON (Node n p a) where
    toJSON (Node _ ndata) =
        case knownPI @p of
            ZI ->
                Aeson.toJSON (unLeaf ndata)
            SI np ->
                withKnownP (Aeson.toJSON (unBranch ndata)) np

instance (Aeson.FromJSON a, HasBBox a) => Aeson.FromJSON (RTreeT n a) where
    parseJSON Aeson.Null =
        pure EmptyT
    parseJSON value = flip (Aeson.withObject "RTree") value $ \obj -> do
        depth <- obj Aeson..: "depth"
        withPIFromInt depth $ \p -> do
            n <- withKnownP (obj Aeson..: "tree") p
            pure $ FullT p n

instance (Aeson.FromJSON a, HasBBox a, KnownP p) => Aeson.FromJSON (Node n p a) where
    parseJSON value =
        case knownPI @p of
            ZI ->
                fmap nodeLeaf $ Aeson.parseJSON value
            SI np ->
                fmap nodeBranch $ withKnownP (Aeson.parseJSON value) np
