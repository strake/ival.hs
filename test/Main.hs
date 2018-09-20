{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (Num (..), Ord (..))
import Algebra
import Control.Applicative
import Data.Interval
import Data.Ratio
import Data.Semigroup (Product)
import Relation.Binary.Comparison as A
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

main :: IO ()
main = defaultMain $
    testGroup ""
    [testProperty "≤-reflexive" $ \ (a :: Interval Rational) -> a ≤ a,
     testProperty "≤-antisymmetric" $ \ (a :: Interval Rational) b -> (a ≤ b && b ≤ a) ≡ (a ≡ b),
     testProperty "≤-transitive" $ \ (a :: Interval Rational) b c -> not (a ≤ b && b ≤ c) || a ≤ c,
     testProperty "≤-∩" $ \ (a :: Interval Rational) b -> (a ≤ b) ≡ (a ∩ b ≡ a)]

instance (Serial m a, PartialOrd a) => Serial m (Interval a) where
    series = decDepth [a :–: b | a <- series, b <- series, a ≤ b]

instance (PartialOrd a, Semigroup (Product a)) => Preord (Ratio a) where
    (liftA2 (,) numerator denominator -> (an, ad)) ≤ (liftA2 (,) numerator denominator -> (bn, bd)) =
        an * bd ≤ bn * ad

instance (A.PartialOrd a, A.Eq a, Semigroup (Product a)) => A.Eq (Ratio a)

instance (PartialOrd a, Semigroup (Product a)) => PartialOrd (Ratio a) where
    tryCompare (liftA2 (,) numerator denominator -> (an, ad)) (liftA2 (,) numerator denominator -> (bn, bd)) =
        tryCompare (an * bd) (bn * ad)

instance (Ord a, Semigroup (Product a)) => Ord (Ratio a) where
    compare (liftA2 (,) numerator denominator -> (an, ad)) (liftA2 (,) numerator denominator -> (bn, bd)) =
        compare (an * bd) (bn * ad)
