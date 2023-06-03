module Data.MultiSet where

import Data.Group.Class
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

newtype MultiSet v = MultiSet
    { multiplicityMap :: Map.Map v Int
    }
    deriving (Show, Eq, Ord)

instance (Ord v) => Semigroup (MultiSet v) where
    MultiSet ms1 <> MultiSet ms2 =
        MultiSet $
            Map.Merge.merge
                Map.Merge.preserveMissing
                Map.Merge.preserveMissing
                (Map.Merge.zipWithMaybeMatched $ \_ n1 n2 -> do
                    let nr = n1 + n2
                    if nr == 0
                        then Nothing
                        else Just nr
                )
                ms1
                ms2

instance (Ord v) => Monoid (MultiSet v) where
    mempty = MultiSet Map.empty

instance (Ord v) => Group (MultiSet v) where
    inverse (MultiSet ms) = MultiSet $ fmap negate ms

empty :: MultiSet v
empty = MultiSet Map.empty

singleton :: v -> MultiSet v
singleton x = MultiSet $ Map.singleton x 1

fromSet :: Set.Set v -> MultiSet v
fromSet s = MultiSet $ Map.fromSet (\_ -> 1) s

toSet :: MultiSet v -> Set.Set v
toSet (MultiSet ms) = Map.keysSet ms

toList :: MultiSet v -> [v]
toList (MultiSet ms) = Map.keys ms

toSeq :: MultiSet v -> Seq.Seq v
toSeq (MultiSet ms) = Map.foldMapWithKey (\v _ -> Seq.singleton v) ms
