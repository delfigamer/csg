{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Group.Class where

import Data.Monoid

class (Monoid a) => Group a where
    inverse :: a -> a

deriving newtype instance (Group a) => Group (Dual a)

instance (Num a) => Group (Sum a) where
    inverse (Sum x) = Sum (negate x)

instance (Fractional a) => Group (Product a) where
    inverse (Product x) = Product (recip x)

instance (Applicative f, Group a) => Group (Ap f a) where
    inverse (Ap fx) = Ap (fmap inverse fx)
