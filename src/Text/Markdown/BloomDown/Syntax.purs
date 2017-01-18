module Text.Markdown.BloomDown.Syntax
  ( BloomDownP(..)
  , BloomDown

  , module BDF
  , module BDI
  , module BDB
  ) where

import Prelude

import Data.List as L
import Data.Monoid (class Monoid, mempty)
import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen

import Text.Markdown.BloomDown.Syntax.Block as BDB
import Text.Markdown.BloomDown.Syntax.FormField as BDF
import Text.Markdown.BloomDown.Syntax.Inline as BDI

-- | `BloomDownP` is the type of BloomDown abstract syntax trees which take values in `a`.
data BloomDownP a = BloomDown (L.List (BDB.Block a))

type BloomDown = BloomDownP String

instance functorBloomDownP ∷ Functor BloomDownP where
  map f (BloomDown bs) = BloomDown (map f <$> bs)

instance showBloomDownP ∷ (Show a) ⇒ Show (BloomDownP a) where
  show (BloomDown bs) = "(BloomDown " <> show bs <> ")"

derive instance eqBloomDownP ∷ (Eq a, Ord a) ⇒ Eq (BloomDownP a)
derive instance ordBloomDownP ∷ (Eq a, Ord a) ⇒ Ord (BloomDownP a)

instance semigroupBloomDownP ∷ Semigroup (BloomDownP a) where
  append (BloomDown bs1) (BloomDown bs2) = BloomDown (bs1 <> bs2)

instance monoidBloomDownP ∷ Monoid (BloomDownP a) where
  mempty = BloomDown mempty

instance arbitraryBloomDownP ∷ (SCA.Arbitrary a, Eq a) ⇒ SCA.Arbitrary (BloomDownP a) where
  arbitrary = BloomDown <<< L.fromFoldable <$> Gen.arrayOf SCA.arbitrary
