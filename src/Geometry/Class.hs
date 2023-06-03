module Geometry.Class where

import Data.Foldable
import Data.Ratio
import Data.String
import qualified Data.Sequence as Seq

type family Coord a

class Vector a where
    (+.) :: a -> a -> a
    (-.) :: a -> a -> a
    (*.) :: Coord a -> a -> a
    negateVec :: a -> a

infixl 6 +.
infixl 6 -.
infixl 7 *.

class Dot a where
    dot :: a -> a -> Coord a

dotSqr :: (Dot a) => a -> Coord a
dotSqr x = dot x x

class Inner a b where
    (*!) :: a -> b -> b

infixl 7 *!

newtype Wolf = Wolf (String -> String)

instance Show Wolf where
    show (Wolf s) = s ""

instance IsString Wolf where
    fromString s = Wolf (s ++)

instance Semigroup Wolf where
    Wolf a <> Wolf b = Wolf (a . b)

instance Monoid Wolf where
    mempty = Wolf id

class ToWolf a where
    toWolf :: a -> Wolf

instance ToWolf Wolf where
    toWolf = id

instance {-# OVERLAPPING #-} ToWolf String where
    toWolf s = Wolf (show s ++)

instance ToWolf Int where
    toWolf i = Wolf (show i ++)

instance ToWolf Integer where
    toWolf i = Wolf (show i ++)

instance ToWolf Rational where
    toWolf r
        | rd == 1 = Wolf (show rn ++)
        | otherwise = Wolf ((show rn ++ "/" ++ show rd) ++)
      where
        rn = numerator r
        rd = denominator r

instance (ToWolf a) => ToWolf [a] where
    toWolf = wolfList . map toWolf

instance (ToWolf a) => ToWolf (Seq.Seq a) where
    toWolf = wolfList . fmap toWolf

wolfList :: (Foldable t) => t Wolf -> Wolf
wolfList = go . toList
  where
    go [] = "{}"
    go (x0:xs0) =
        "{" <> x0 <> foldMap (\x -> "," <> x) xs0 <> "}"

wolfApp :: (Foldable t) => Wolf -> t Wolf -> Wolf
wolfApp fn = go . toList
  where
    go [] = fn <> "[]"
    go (x0:xs0) =
        fn <> "[" <> x0 <> foldMap (\x -> "," <> x) xs0 <> "]"

wolfApp2 :: (Foldable t, Foldable u) => Wolf -> t Wolf -> u (Wolf, Wolf) -> Wolf
wolfApp2 fn p n =
    wolfApp fn $ toList p <> fmap (\(k,v) -> k <> "->" <> v) (toList n)

class ToWolfMod a where
    toWolfMod :: a -> [Wolf]

instance ToWolfMod () where
    toWolfMod _ = []

instance ToWolfMod Wolf where
    toWolfMod w = [w]

instance ToWolfMod [Wolf] where
    toWolfMod = id
