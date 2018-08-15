module Data.Interval (Interval (..), (∪), (∩)) where

import Prelude hiding (Eq, Ord (..), max, min)

import Control.Applicative
import Data.Function (on)
import Data.Ord (Down (..))
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

infixr 4 ∪, ∩
(∪), (∩) :: Ord a => Interval a -> Interval a -> Interval a
a₁ :–: b₁ ∪ a₂ :–: b₂ = liftA2 min a₁ a₂ :–: liftA2 max b₁ b₂
a₁ :–: b₁ ∩ a₂ :–: b₂ = fmap unMax (fmap Max a₁ <> fmap Max a₂) :–:
                        fmap unMin (fmap Min b₁ <> fmap Min b₂)
