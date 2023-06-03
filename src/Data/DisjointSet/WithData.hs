module Data.DisjointSet.WithData
    ( DisjointSet
    , newDisjointSet
    , findSubsetRoot
    , unifySubsets
    , collectRoots
    )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as Boxed.Vector
import qualified Data.Vector.Mutable as Boxed.MVector
import qualified Data.Vector.Unboxed.Mutable as Unboxed.MVector

data DisjointSet s a = DisjointSet
    (Unboxed.MVector.MVector s Int)
    (Unboxed.MVector.MVector s Int)
    (Boxed.MVector.MVector s a)

newDisjointSet ::
    Boxed.Vector.Vector a ->
    ST s (DisjointSet s a)
newDisjointSet args = do
    let size = Boxed.Vector.length args
    DisjointSet
        <$> Unboxed.MVector.generate size id
        <*> Unboxed.MVector.replicate size 0
        <*> Boxed.Vector.thaw args

findSubsetRoot :: DisjointSet s a -> Int -> ST s Int
findSubsetRoot (DisjointSet parents _ _) =
    go
  where
    go i = do
        iparent <- Unboxed.MVector.read parents i
        if i == iparent
            then pure i
            else do
                rep <- go iparent
                Unboxed.MVector.write parents i rep
                pure rep

unifySubsets ::
    (Semigroup a) =>
    DisjointSet s a ->
    Int ->
    Int ->
    ST s ()
unifySubsets dset@(DisjointSet parents ranks values) a b = do
    arep <- findSubsetRoot dset a
    brep <- findSubsetRoot dset b
    unless (arep == brep) $ do
        arepRank <- Unboxed.MVector.read ranks arep
        brepRank <- Unboxed.MVector.read ranks brep
        arepValue <- Boxed.MVector.read values arep
        brepValue <- Boxed.MVector.read values brep
        let !newValue =
                if arep < brep
                    then arepValue <> brepValue
                    else brepValue <> arepValue
        if
            | arepRank < brepRank -> do
                Unboxed.MVector.write parents arep brep
                Boxed.MVector.write values brep newValue
            | arepRank > brepRank -> do
                Unboxed.MVector.write parents brep arep
                Boxed.MVector.write values arep newValue
            | otherwise -> do
                Unboxed.MVector.write parents arep brep
                Unboxed.MVector.write ranks brep (brepRank + 1)
                Boxed.MVector.write values brep newValue

collectRoots :: DisjointSet s a -> ST s [(Int, a)]
collectRoots (DisjointSet parents _ values) =
    go (Unboxed.MVector.length parents - 1) []
  where
    go !i xs
        | i < 0 = pure xs
        | otherwise = do
            iparent <- Unboxed.MVector.read parents i
            if iparent == i
                then do
                    ivalue <- Boxed.MVector.read values i
                    go (i - 1) ((i, ivalue) : xs)
                else do
                    go (i - 1) xs
