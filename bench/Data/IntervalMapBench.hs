module Data.IntervalMapBench where
{-
import Control.DeepSeq
import Control.Monad
-- import Criterion.Main
import Data.Interval
import Data.Time
import qualified Data.IntervalMap.ITree as ITree
import qualified Data.IntervalMap.ITreeUnboxed as ITreeUnboxed
import qualified Data.IntervalMap.RangeMap as RangeMap
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified System.Random as Random
import qualified System.Random.Stateful as Random.Stateful

newtype MultiSet v = MultiSet (Map.Map v Int)
    deriving (Show, Eq, Ord)

instance (Ord v) => Semigroup (MultiSet v) where
    MultiSet vm1 <> MultiSet vm2 =
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
                vm1
                vm2

instance (Ord v) => Monoid (MultiSet v) where
    mempty = MultiSet Map.empty

instance (Ord v) => RangeMap.Group (MultiSet v) where
    inverse (MultiSet ms) = MultiSet $ fmap negate ms

singletonMultiset :: v -> MultiSet v
singletonMultiset x = MultiSet $ Map.singleton x 1



generateRanges :: Int -> IO [(Interval Int, Int)]
generateRanges n = do
    g <- Random.newStdGen >>= Random.Stateful.newIOGenM
    replicateM n $ do
        a <- Random.Stateful.randomRM (0, n * 40) g
        len <- Random.Stateful.randomRM (1, 100) g
        x <- Random.Stateful.randomM g
        pure (Interval a (a + len), x)

generateFromRangeMap :: Int -> IO [(Int, Int, Map.Map Int Int)]
generateFromRangeMap n = do
    intervals <- generateRanges n
    let rangeMap =
            foldMap
                (\(ab, c) -> RangeMap.singleton ab (singletonMultiset c))
                intervals
    pure $
        fmap (\(Interval a b, MultiSet c) -> (a, b, c)) $
            RangeMap.toIntervalList rangeMap

generateFromITree :: Int -> IO [(Int, Int, Map.Map Int Int)]
generateFromITree n = do
    intervals <- generateRanges n
    let itree =
            foldMap
                (\(ab, c) -> ITree.singleton ab (singletonMultiset c))
                intervals
    pure $
        fmap (\(Interval a b, MultiSet c) -> (a, b, c)) $
            ITree.toIntervalList itree

generateFromITreeUnboxed :: Int -> IO [(Int, Int, Map.Map Int Int)]
generateFromITreeUnboxed n = do
    intervals <- generateRanges n
    let itree =
            foldMap
                (\(ab, c) -> ITreeUnboxed.singleton ab (singletonMultiset c))
                intervals
    pure $
        fmap (\(Interval a b, MultiSet c) -> (a, b, c)) $
            ITreeUnboxed.toIntervalList itree

-- benchmark :: Benchmark
-- benchmark = bgroup "Data.IntervalMap"
    -- [ bgroup "RangeMap"
        -- [ bench "10" $      nfIO $ generateFromRangeMap 10
        -- , bench "100" $     nfIO $ generateFromRangeMap 100
        -- , bench "1000" $    nfIO $ generateFromRangeMap 1000
        -- , bench "10000" $   nfIO $ generateFromRangeMap 10000
        -- [ bench "100000" $  nfIO $ generateFromRangeMap 100000
        -- , bench "1000000" $ nfIO $ generateFromRangeMap 1000000
        -- ]
    -- , bgroup "ITree"
        -- [ bench "10" $      nfIO $ generateFromITree 10
        -- , bench "100" $     nfIO $ generateFromITree 100
        -- , bench "1000" $    nfIO $ generateFromITree 1000
        -- , bench "10000" $   nfIO $ generateFromITree 10000
        -- [ bench "100000" $  nfIO $ generateFromITree 100000
        -- , bench "1000000" $ nfIO $ generateFromITree 1000000
        -- ]
    -- ]

measure :: (NFData a) => IO a -> IO NominalDiffTime
measure act = do
    t1 <- getCurrentTime
    r <- act
    _ <- pure $!! r
    t2 <- getCurrentTime
    pure $ diffUTCTime t2 t1

runBenchmark :: IO ()
runBenchmark = do
    iter 0 0 0 0
  where
    iter n itime utime rtime = do
        it <- measure $ generateFromITree 100000
        ut <- measure $ generateFromITreeUnboxed 100000
        rt <- measure $ generateFromRangeMap 100000
        let n2 = n + 1
        let itime2 = itime + it
        let utime2 = utime + ut
        let rtime2 = rtime + rt
        putStrLn $ "Iteration " <> show n2
        putStrLn $ "ITree time:         " <> show (itime2 / fromInteger n2)
        putStrLn $ "ITree unboxed time: " <> show (utime2 / fromInteger n2)
        putStrLn $ "RangeMap time:      " <> show (rtime2 / fromInteger n2)
        iter n2 itime2 utime2 rtime2
-}
