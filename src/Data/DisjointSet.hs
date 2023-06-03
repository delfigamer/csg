module Data.DisjointSet
    ( DisjointSet
    , getDisjointSetRoots
    , newDisjointSet
    , findSubsetRoot
    , unifySubsets
    )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as Unboxed.MVector

data DisjointSet s = DisjointSet
    (Unboxed.MVector.MVector s Int)
    (Unboxed.MVector.MVector s Int)

getDisjointSetRoots :: DisjointSet s -> ST s [Int]
getDisjointSetRoots (DisjointSet parents _) =
    go id 0
  where
    go buf i
        | i < Unboxed.MVector.length parents = do
            iparent <- Unboxed.MVector.read parents i
            if i == iparent
                then go (buf . (i :)) (i + 1)
                else go buf (i + 1)
        | otherwise =
            pure $ buf []

newDisjointSet :: Int -> ST s (DisjointSet s)
newDisjointSet size =
    DisjointSet
        <$> Unboxed.MVector.generate size id
        <*> Unboxed.MVector.replicate size 0

findSubsetRoot :: DisjointSet s -> Int -> ST s Int
findSubsetRoot (DisjointSet parents _) =
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

unifySubsets :: DisjointSet s -> Int -> Int -> ST s ()
unifySubsets dset@(DisjointSet parents ranks) a b = do
    arep <- findSubsetRoot dset a
    brep <- findSubsetRoot dset b
    unless (arep == brep) $ do
        arepRank <- Unboxed.MVector.read ranks arep
        brepRank <- Unboxed.MVector.read ranks brep
        if
            | arepRank < brepRank ->
                Unboxed.MVector.write parents arep brep
            | arepRank > brepRank ->
                Unboxed.MVector.write parents brep arep
            | otherwise -> do
                Unboxed.MVector.write parents arep brep
                Unboxed.MVector.write ranks brep (brepRank + 1)
