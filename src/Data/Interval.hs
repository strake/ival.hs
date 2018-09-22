module Data.Interval (Interval (..), size, overlap, hull, isPoint) where

import Prelude hiding (Eq, Ord (..), Num (..), max, min, null)

import Algebra
import Control.Applicative
import Control.Monad (guard)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Semigroup (Sum (..))
import Relation.Binary.Comparison

infix 5 :–:
data Interval a = Maybe a :–: Maybe a
    deriving (Read, Show,
              Functor, Foldable, Traversable)

instance Preord a => Preord (Interval a) where
    a₁ :–: b₁ ≤ a₂ :–: b₂ = Lexical a₁ ≥ Lexical a₂ && ((≤) `on` Down . Lexical . fmap Down) b₁ b₂
instance PartialEq a => PartialEq (Interval a) where a₁ :–: b₁ ≡ a₂ :–: b₂ = (a₁, b₁) ≡ (a₂, b₂)
instance (PartialOrd a, PartialEq a) => PartialOrd (Interval a)
instance Eq a => Eq (Interval a)

size :: Group (Sum a) => Interval a -> Maybe a
size (a :–: b) = liftA2 (-) b a

overlap :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
overlap (a₁ :–: b₁) (a₂ :–: b₂) = z <$ (guard . not . null) z
  where z = fmap unMax (fmap Max a₁ <> fmap Max a₂) :–:
            fmap unMin (fmap Min b₁ <> fmap Min b₂)

hull :: Ord a => Interval a -> Interval a -> Interval a
hull (a₁ :–: b₁) (a₂ :–: b₂) = liftA2 min a₁ a₂ :–: liftA2 max b₁ b₂

null :: Ord a => Interval a -> Bool
null (a :–: b) = fromMaybe False $ liftA2 (>) a b

isPoint :: Eq a => Interval a -> Maybe a
isPoint (a :–: b) = a <* guard (a ≡ b)
