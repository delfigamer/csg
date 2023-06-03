{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.ShortVector
    ( ShortVector
    , empty
    , singleton
    , (!!)
    , set
    , modify
    , append
    , take
    , drop
    , null
    , length
    , fromList
    , fromListN
    , Foldable.toList
    , foldMapWithIndex
    , foldMapWithIndex'
    , foldrWithIndex
    , foldlWithIndex'
    , sortOn
    , sortBy
    )
where

import Prelude hiding ((!!), drop, length, take)
import qualified Control.Applicative as Applicative
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Semigroup as Semigroup
import GHC.Exts
import GHC.ST
import Unsafe.Coerce

data ShortVector a
    = ShortVector# (SmallArray# a)

data MutableShortVector s a
    = MutableShortVector# (SmallMutableArray# s a)

undefinedElement :: a
undefinedElement =
    errorWithoutStackTrace "undefined short vector element"

constructShortVector ::
    Int ->
    a ->
    (forall s. MutableShortVector s a -> ST s ()) ->
    ShortVector a
constructShortVector (I# n#) a fn =
    runRW# $ \s1 ->
        case newSmallArray# n# a s1 of
            (# s2, sma# #) ->
                case fn (MutableShortVector# sma#) of
                    ST strep ->
                        case strep s2 of
                            (# s3, _ #) ->
                                case unsafeFreezeSmallArray# sma# s3 of
                                    (# _, sa# #) ->
                                        ShortVector# sa#

updateShortVector ::
    ShortVector a ->
    Int ->
    Int ->
    (forall s. MutableShortVector s a -> ST s ()) ->
    ShortVector a
updateShortVector (ShortVector# saSrc#) (I# i#) (I# n#) fn =
    runRW# $ \s1 ->
        case thawSmallArray# saSrc# i# n# s1 of
            (# s2, sma# #) ->
                case fn (MutableShortVector# sma#) of
                    ST strep ->
                        case strep s2 of
                            (# s3, _ #) ->
                                case unsafeFreezeSmallArray# sma# s3 of
                                    (# _, sa# #) ->
                                        ShortVector# sa#

empty :: ShortVector a
empty = emptyShortVector

singleton ::
    a ->
    ShortVector a
singleton a =
    runRW# $ \s1 ->
        case newSmallArray# 1# a s1 of
            (# s2, sma# #) ->
                case unsafeFreezeSmallArray# sma# s2 of
                    (# _, sa# #) ->
                        ShortVector# sa#

unsafeReadMutableShortVector :: MutableShortVector s a -> Int -> ST s a
unsafeReadMutableShortVector (MutableShortVector# sma#) (I# n#) =
    ST $ \s1 ->
        readSmallArray# sma# n# s1

unsafeWriteMutableShortVector :: MutableShortVector s a -> Int -> a -> ST s ()
unsafeWriteMutableShortVector (MutableShortVector# sma#) (I# n#) a =
    ST $ \s1 ->
        (# writeSmallArray# sma# n# a s1, () #)

unsafeCopyShortVector ::
    ShortVector a -> Int ->
    MutableShortVector s a -> Int ->
    Int ->
    ST s ()
unsafeCopyShortVector
    (ShortVector# src#) (I# srcI#)
    (MutableShortVector# dst#) (I# dstI#)
    (I# n#)
  =
    ST $ \s1 ->
        (# copySmallArray# src# srcI# dst# dstI# n# s1, () #)

unsafeIndexShortVector :: ShortVector a -> Int -> a
unsafeIndexShortVector (ShortVector# sa#) (I# n#) =
    case indexSmallArray# sa# n# of
        (# x #) -> x

emptyShortVector :: ShortVector a
emptyShortVector =
    unsafeCoerce emptyShortVectorAny
  where
    emptyShortVectorAny :: ShortVector Any
    emptyShortVectorAny =
        runRW# $ \s1 ->
            case newSmallArray# 0# undefined s1 of
                (# s2, sma# #) ->
                    case unsafeFreezeSmallArray# sma# s2 of
                        (# _, sa# #) ->
                            ShortVector# sa#

length :: ShortVector a -> Int
length (ShortVector# sa#) =
    I# (sizeofSmallArray# sa#)

data ViewMapping a b where
    ViewMappingId :: ViewMapping a a
    ViewMappingFn :: (a -> b) -> ViewMapping a b

applyMapping :: ViewMapping a b -> a -> b
applyMapping ViewMappingId = id
applyMapping (ViewMappingFn fn) = fn

data ShortVectorView a
    = forall t. ShortVectorView
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !Int
        !(ViewMapping t a)
        {-# UNPACK #-} !(ShortVector t)

makeView ::
    ShortVector a ->
    ShortVectorView a
makeView source =
    ShortVectorView 0 (length source) ViewMappingId source
{-# NOINLINE [1] makeView #-}

materializeView ::
    ShortVectorView a ->
    ShortVector a
materializeView (ShortVectorView begin end mapping source) =
    case mapping of
        ViewMappingId ->
            updateShortVector source begin (end - begin) $ \_ -> pure ()
        ViewMappingFn fn ->
            constructShortVector (end - begin) undefinedElement $ \msv -> do
                Foldable.forM_ [begin .. end - 1] $ \i ->
                    unsafeWriteMutableShortVector msv i $
                        fn $ unsafeIndexShortVector source i
{-# NOINLINE [1] materializeView #-}

{-# RULES "shortVector/make-materialize" forall x. makeView (materializeView x) = x #-}

takeView ::
    Int ->
    ShortVectorView a ->
    ShortVectorView a
takeView n (ShortVectorView begin end mapping source)
    | newEnd < begin =
        ShortVectorView begin begin mapping source
    | newEnd > end =
        ShortVectorView begin end mapping source
    | otherwise =
        ShortVectorView begin newEnd mapping source
  where
    newEnd = begin + n

dropView ::
    Int ->
    ShortVectorView a ->
    ShortVectorView a
dropView n (ShortVectorView begin end mapping source)
    | newBegin < begin =
        ShortVectorView begin end mapping source
    | newBegin > end =
        ShortVectorView end end mapping source
    | otherwise =
        ShortVectorView newBegin end mapping source
  where
    newBegin = begin + n

take :: Int -> ShortVector a -> ShortVector a
take n = materializeView . takeView n . makeView

drop :: Int -> ShortVector a -> ShortVector a
drop n = materializeView . dropView n . makeView

mapView ::
    (a -> b) ->
    ShortVectorView a ->
    ShortVectorView b
mapView newFn (ShortVectorView begin end mapping source) =
    ShortVectorView begin end newMapping source
  where
    newMapping =
        case mapping of
            ViewMappingId -> ViewMappingFn newFn
            ViewMappingFn oldFn -> ViewMappingFn (newFn . oldFn)

foldrViewWithIndex :: (Int -> a -> b -> b) -> b -> ShortVectorView a -> b
foldrViewWithIndex fn b0 (ShortVectorView begin end mapping source) =
    go begin
  where
    go i
        | i < end =
            fn
                (i - begin)
                (applyMapping mapping $ unsafeIndexShortVector source i)
                (go (i + 1))
        | otherwise =
            b0

foldrView :: (a -> b -> b) -> b -> ShortVectorView a -> b
foldrView fn b0 (ShortVectorView begin end mapping source) =
    go begin
  where
    go i
        | i < end =
            fn
                (applyMapping mapping $ unsafeIndexShortVector source i)
                (go (i + 1))
        | otherwise =
            b0

foldrView' :: (a -> b -> b) -> b -> ShortVectorView a -> b
foldrView' fn b0 (ShortVectorView begin end mapping source) =
    go (end - 1) b0
  where
    go i b
        | i >= begin =
            go (i - 1) $!
                fn
                    (applyMapping mapping $ unsafeIndexShortVector source i)
                    b
        | otherwise =
            b

foldlView :: (b -> a -> b) -> b -> ShortVectorView a -> b
foldlView fn b0 (ShortVectorView begin end mapping source) =
    go (end - 1)
  where
    go i
        | i >= begin =
            fn
                (go (i - 1))
                (applyMapping mapping $ unsafeIndexShortVector source i)
        | otherwise =
            b0

foldlViewWithIndex' :: (Int -> b -> a -> b) -> b -> ShortVectorView a -> b
foldlViewWithIndex' fn b0 (ShortVectorView begin end mapping source) =
    go begin b0
  where
    go i b
        | i < end =
            go (i + 1) $!
                fn
                    (i - begin)
                    b
                    (applyMapping mapping $ unsafeIndexShortVector source i)
        | otherwise =
            b

foldlView' :: (b -> a -> b) -> b -> ShortVectorView a -> b
foldlView' fn b0 (ShortVectorView begin end mapping source) =
    go begin b0
  where
    go i b
        | i < end =
            go (i + 1) $!
                fn
                    b
                    (applyMapping mapping $ unsafeIndexShortVector source i)
        | otherwise =
            b

nullView :: ShortVectorView a -> Bool
nullView (ShortVectorView begin end _ _) =
    end > begin

lengthView :: ShortVectorView a -> Int
lengthView (ShortVectorView begin end _ _) =
    end - begin

foldrWithIndex ::
    (Int -> a -> b -> b) ->
    b ->
    ShortVector a ->
    b
foldrWithIndex fn b0 =
    foldrViewWithIndex fn b0 . makeView

foldlWithIndex' ::
    (Int -> b -> a -> b) ->
    b ->
    ShortVector a ->
    b
foldlWithIndex' fn b0 =
    foldlViewWithIndex' fn b0 . makeView

foldMapWithIndex ::
    (Monoid m) =>
    (Int -> a -> m) ->
    ShortVector a ->
    m
foldMapWithIndex fn =
    foldrWithIndex (\i x b -> fn i x <> b) mempty

foldMapWithIndex' ::
    (Monoid m) =>
    (Int -> a -> m) ->
    ShortVector a ->
    m
foldMapWithIndex' fn =
    foldlWithIndex' (\i b x -> b <> fn i x) mempty

(!!) :: ShortVector a -> Int -> a
(!!) sv i
    | i >= 0 && i < length sv =
        unsafeIndexShortVector sv i
    | otherwise =
        errorWithoutStackTrace "invalid index"

set ::
    Int ->
    a ->
    ShortVector a ->
    ShortVector a
set n x source
    | n >= 0 && n < length source =
        updateShortVector source 0 (length source) $ \msv -> do
            unsafeWriteMutableShortVector msv n x
    | otherwise =
        source

modify ::
    Int ->
    (a -> a) ->
    ShortVector a ->
    ShortVector a
modify n fn source
    | n >= 0 && n < length source =
        updateShortVector source 0 (length source) $ \msv -> do
            unsafeWriteMutableShortVector msv n $
                fn $ unsafeIndexShortVector source n
    | otherwise =
        source

copyFromView :: ShortVectorView a -> MutableShortVector s a -> Int -> ST s ()
copyFromView (ShortVectorView begin end mapping source) msv pos = do
    case mapping of
        ViewMappingId ->
            unsafeCopyShortVector source begin msv pos (end - begin)
        ViewMappingFn fn ->
            Foldable.forM_ [0 .. (end - begin) - 1] $ \i -> do
                unsafeWriteMutableShortVector msv (pos + i) $
                    fn $ unsafeIndexShortVector source (begin + i)

appendFromView ::
    ShortVectorView a ->
    a ->
    ShortVector a
appendFromView view x =
    constructShortVector (lengthView view + 1) x $ \msv -> do
        copyFromView view msv 0

append ::
    ShortVector a ->
    a ->
    ShortVector a
append sv x =
    appendFromView (makeView sv) x

mappendFromView ::
    ShortVectorView a ->
    ShortVectorView a ->
    ShortVector a
mappendFromView view1 view2 =
    constructShortVector (size1 + size2) undefinedElement $ \msv -> do
        copyFromView view1 msv 0
        copyFromView view2 msv size1
  where
    size1 = lengthView view1
    size2 = lengthView view2

stimesFromView ::
    (Integral n) => n -> ShortVectorView a -> ShortVector a
stimesFromView n view =
    constructShortVector totalN undefinedElement $ \msv -> do
        let go i
                | i < totalN = do
                    copyFromView view msv i
                    go (i + size)
                | otherwise = do
                    pure ()
        go 0
  where
    totalN = size * fromIntegral n
    size = lengthView view

mconcatFromView :: [ShortVectorView a] -> ShortVector a
mconcatFromView viewList =
    constructShortVector totalN undefinedElement $ \msv -> do
        let go (v : vRest) i = do
                copyFromView v msv i
                go vRest (i + lengthView v)
            go [] _ = do
                pure ()
        go viewList 0
  where
    totalN =
        Semigroup.getSum $
            Foldable.foldMap'
                (\v -> Semigroup.Sum $ lengthView v)
                viewList

liftA2FromViews ::
    (a -> b -> c) ->
    ShortVectorView a ->
    ShortVectorView b ->
    ShortVector c
liftA2FromViews
    fn
    (ShortVectorView begin1 end1 mapping1 source1)
    (ShortVectorView begin2 end2 mapping2 source2)
  | size1 == 0 || size2 == 0 =
    emptyShortVector
  | otherwise =
    constructShortVector (size1 * size2) undefinedElement $ \msv -> do
        let go i j k
                | i >= end1 = do
                    pure ()
                | j >= end2 = do
                    go (i + 1) begin2 k
                | otherwise = do
                    unsafeWriteMutableShortVector msv k $
                        fn
                            (applyMapping mapping1 $ unsafeIndexShortVector source1 i)
                            (applyMapping mapping2 $ unsafeIndexShortVector source2 j)
                    go i (j + 1) (k + 1)
        go begin1 begin2 0
  where
    size1 = end1 - begin1
    size2 = end2 - begin2

instance IsList (ShortVector a) where
    type Item (ShortVector a) = a
    fromList xs0 =
        fromListN (Foldable.length xs0) xs0
    fromListN n xs0 =
        constructShortVector n undefinedElement $ \msv -> do
            let go i (x : xs)
                    | i < n = do
                        unsafeWriteMutableShortVector msv i x
                        go (i + 1) xs
                go _ _ = do
                    pure ()
            go 0 xs0
    toList = Foldable.toList

instance Foldable ShortVector where
    foldr fn b0 =
        foldrView fn b0 . makeView
    foldr' fn b0 =
        foldrView' fn b0 . makeView
    foldl fn b0 =
        foldlView fn b0 . makeView
    foldl' fn b0 =
        foldlView' fn b0 . makeView
    null =
        nullView . makeView
    length =
        lengthView . makeView

instance (Show a) => Show (ShortVector a) where
    showsPrec d sv =
        showParen (d > 10) $
            showString "fromList " .
            showsPrec 11 (Foldable.toList sv)

instance Functor ShortVector where
    fmap fn =
        materializeView . mapView fn . makeView

instance Traversable ShortVector where
    traverse fn =
        (\view -> fmap (fromListN (lengthView view)) $ traverseViewIntoList view) .
        makeView
      where
        traverseViewIntoList =
            foldrView
                (\x rest -> Applicative.liftA2 (:) (fn x) rest)
                (pure [])

instance Applicative ShortVector where
    pure = singleton
    liftA2 fn sv1 sv2 =
        liftA2FromViews fn (makeView sv1) (makeView sv2)
    (*>) sv1 sv2 =
        Semigroup.stimes (length sv1) sv2

instance Monad ShortVector where
    sv >>= fn =
        mconcat $ foldr (\x rs -> fn x : rs) [] sv
    (>>) = (*>)

instance Semigroup (ShortVector a) where
    (<>) = mappend
    sconcat = mconcat . Foldable.toList
    stimes n sv = stimesFromView n (makeView sv)

instance Monoid (ShortVector a) where
    mempty = emptyShortVector
    mappend sv1 sv2 =
        mappendFromView (makeView sv1) (makeView sv2)
    mconcat svList =
        mconcatFromView (fmap makeView svList)

instance (Aeson.ToJSON a) => Aeson.ToJSON (ShortVector a) where
    toJSON sv = Aeson.toJSON $ Foldable.toList sv

instance (Aeson.FromJSON a) => Aeson.FromJSON (ShortVector a) where
    parseJSON = fmap fromList . Aeson.parseJSON

sortOn ::
    (Ord b) =>
    (a -> b) ->
    ShortVector a ->
    ShortVector a
sortOn fn =
    sortBy (\x y -> fn x <= fn y)

sortBy ::
    (a -> a -> Bool) ->
    ShortVector a ->
    ShortVector a
sortBy le source =
    constructShortVector (length source) undefinedElement $ \msv -> do
        buildSorted le source msv

buildSorted ::
    (a -> a -> Bool) ->
    ShortVector a ->
    MutableShortVector s a ->
    ST s ()
buildSorted le source msv = do
    forMI_ (length source) $ \i -> do
        heapRaise i (unsafeIndexShortVector source i)
    forMReverseI_ (length source) $ \i -> do
        xmax <- unsafeReadMutableShortVector msv 0
        xt <- unsafeReadMutableShortVector msv i
        unsafeWriteMutableShortVector msv i xmax
        heapLower i 0 xt
  where
    heapRaise i x
        | i == 0 = do
            unsafeWriteMutableShortVector msv i x
        | otherwise = do
            let ip = (i - 1) `quot` 2
            xp <- unsafeReadMutableShortVector msv ip
            if x `le` xp
                then do
                    unsafeWriteMutableShortVector msv i x
                else do
                    unsafeWriteMutableShortVector msv i xp
                    heapRaise ip x
    heapLower n i x = do
        let ic1 = 2 * i + 1
        let ic2 = 2 * i + 2
        if
            | ic1 >= n -> do
                unsafeWriteMutableShortVector msv i x
            | ic2 >= n -> do
                xc1 <- unsafeReadMutableShortVector msv ic1
                heapLowerInto n i x ic1 xc1
            | otherwise -> do
                xc1 <- unsafeReadMutableShortVector msv ic1
                xc2 <- unsafeReadMutableShortVector msv ic2
                if xc1 `le` xc2
                    then do
                        heapLowerInto n i x ic2 xc2
                    else do
                        heapLowerInto n i x ic1 xc1
    heapLowerInto n i x ic xc = do
        if xc `le` x
            then do
                unsafeWriteMutableShortVector msv i x
            else do
                unsafeWriteMutableShortVector msv i xc
                heapLower n ic x

forMI_ ::
    (Applicative m) =>
    Int ->
    (Int -> m ()) ->
    m ()
forMI_ n fn =
    go 0
  where
    go i
        | i < n =
            fn i *> go (i + 1)
        | otherwise =
            pure ()

forMReverseI_ ::
    (Applicative m) =>
    Int ->
    (Int -> m ()) ->
    m ()
forMReverseI_ n fn =
    go (n - 1)
  where
    go i
        | i >= 0 =
            fn i *> go (i - 1)
        | otherwise =
            pure ()
