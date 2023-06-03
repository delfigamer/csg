module Data.CList where

foldrPairsInner :: (a -> a -> b -> b) -> b -> [a] -> b
foldrPairsInner _ seed [] =
    seed
foldrPairsInner f seed (x0 : xs0) =
    go x0 xs0
  where
    go _ [] = seed
    go prev (x : xs) = f prev x (go x xs)

foldrPairsLoop :: (a -> a -> b -> b) -> b -> [a] -> b
foldrPairsLoop _ seed [] =
    seed
foldrPairsLoop f seed (x0 : xs0) =
    go x0 xs0
  where
    go prev [] = f prev x0 seed
    go prev (x : xs) = f prev x (go x xs)

mapPairsLoop :: (a -> a -> b) -> [a] -> [b]
mapPairsLoop f = foldrPairsLoop (\x y rs -> f x y : rs) []

foldMapPairsInner :: (Monoid m) => (a -> a -> m) -> [a] -> m
foldMapPairsInner f = foldrPairsInner (\x y rs -> f x y <> rs) mempty

foldMapPairsLoop :: (Monoid m) => (a -> a -> m) -> [a] -> m
foldMapPairsLoop f = foldrPairsLoop (\x y rs -> f x y <> rs) mempty

trySelectMinimumOn :: (Ord b) => (a -> b) -> [a] -> Maybe a
trySelectMinimumOn f xs =
    fmap fst (trySelectMinimumOnHelper f xs)

trySelectMinimumOnHelper :: (Ord b) => (a -> b) -> [a] -> Maybe (a, b)
trySelectMinimumOnHelper _ [] = Nothing
trySelectMinimumOnHelper f (x : xs) =
    case trySelectMinimumOnHelper f xs of
        Nothing -> Just (x, f x)
        Just (y, oy) -> do
            let ox = f x
            if ox <= oy
                then Just (x, ox)
                else Just (y, oy)

tryTakeMinimumOn :: (Ord b) => (a -> b) -> [a] -> Maybe (a, [a])
tryTakeMinimumOn f xs = case tryTakeMinimumOnHelper f xs of
    Just (y, _, ys) -> Just (y, ys)
    Nothing -> Nothing

tryTakeMinimumOnHelper :: (Ord b) => (a -> b) -> [a] -> Maybe (a, b, [a])
tryTakeMinimumOnHelper _ [] = Nothing
tryTakeMinimumOnHelper f (x : xs) =
    case tryTakeMinimumOnHelper f xs of
        Nothing -> Just (x, f x, [])
        Just (y, oy, ys) -> do
            let ox = f x
            if ox <= oy
                then Just (x, ox, y : ys)
                else Just (y, oy, x : ys)

nubConsecutive :: (Eq a) => [a] -> [a]
nubConsecutive [] = []
nubConsecutive (a : as) = a : go a as
  where
    go _ [] = []
    go x (y : ys)
        | x == y = go x ys
        | otherwise = y : go y ys
