module Text.Markdown.BloomDown.Traverse
  ( everywhereM
  , everywhere
  , everywhereTopDownM
  , everywhereTopDown
  , everythingM
  , everything
  ) where

import Prelude

import Data.Foldable as F
import Data.Identity as Id
import Data.Monoid (class Monoid)
import Data.Newtype (un)
import Data.Traversable as T

import Text.Markdown.BloomDown.Syntax as BD

everywhereM
  ∷ ∀ m a
  . (Monad m)
  ⇒ (BD.Block a → m (BD.Block a))
  → (BD.Inline a → m (BD.Inline a))
  → BD.BloomDownP a
  → m (BD.BloomDownP a)
everywhereM b i (BD.BloomDown bs) =
  BD.BloomDown <$> T.traverse b' bs

  where
  b' ∷ BD.Block a → m (BD.Block a)
  b' (BD.Paragraph is) = (BD.Paragraph <$> T.traverse i' is) >>= b
  b' (BD.Header n is) = (BD.Header n <$> T.traverse i' is) >>= b
  b' (BD.Blockquote bs') = (BD.Blockquote <$> T.traverse b' bs') >>= b
  b' (BD.Lst lt bss) = (BD.Lst lt <$> T.traverse (T.traverse b') bss) >>= b
  b' other = b other

  i' ∷ BD.Inline a → m (BD.Inline a)
  i' (BD.Emph is) = (BD.Emph <$> T.traverse i' is) >>= i
  i' (BD.Strong is) = (BD.Strong <$> T.traverse i' is) >>= i
  i' (BD.Link is uri) = (flip BD.Link uri <$> T.traverse i' is) >>= i
  i' (BD.Image is uri) = (flip BD.Image uri <$> T.traverse i' is) >>= i
  i' other = i other

everywhere
  ∷ ∀ a
  . (BD.Block a → BD.Block a)
  → (BD.Inline a → BD.Inline a)
  → BD.BloomDownP a
  → BD.BloomDownP a
everywhere b i =
  un Id.Identity
    <<< everywhereM (pure <<< b) (pure <<< i)

everywhereTopDownM
  ∷ ∀ m a
  . (Monad m)
  ⇒ (BD.Block a → m (BD.Block a))
  → (BD.Inline a → m (BD.Inline a))
  → BD.BloomDownP a
  → m (BD.BloomDownP a)
everywhereTopDownM b i (BD.BloomDown bs) =
  BD.BloomDown <$>
    T.traverse (b' <=< b) bs
  where
  b' ∷ BD.Block a → m (BD.Block a)
  b' (BD.Paragraph is) = BD.Paragraph <$> T.traverse (i' <=< i) is
  b' (BD.Header n is) = BD.Header n <$> T.traverse (i' <=< i) is
  b' (BD.Blockquote bs') = BD.Blockquote <$> T.traverse (b' <=< b) bs'
  b' (BD.Lst ty bss) = BD.Lst ty <$> T.traverse (T.traverse (b' <=< b)) bss
  b' other = b other

  i' ∷ BD.Inline a → m (BD.Inline a)
  i' (BD.Emph is) = BD.Emph <$> T.traverse (i' <=< i) is
  i' (BD.Strong is) = BD.Strong <$> T.traverse (i' <=< i) is
  i' (BD.Link is uri) = flip BD.Link uri <$> T.traverse (i' <=< i) is
  i' (BD.Image is uri) = flip BD.Image uri <$> T.traverse (i' <=< i) is
  i' other = i other

everywhereTopDown
  ∷ ∀ a
  . (BD.Block a → BD.Block a)
  → (BD.Inline a → BD.Inline a)
  → BD.BloomDownP a
  → BD.BloomDownP a
everywhereTopDown b i =
  un Id.Identity <<<
    everywhereTopDownM
      (pure <<< b)
      (pure <<< i)

everythingM
  ∷ ∀ m a r
  . (Monad m, Monoid r)
  ⇒ (BD.Block a → m r)
  → (BD.Inline a → m r)
  → BD.BloomDownP a
  → m r
everythingM b i (BD.BloomDown bs) =
  F.fold <$> T.traverse b' bs
  where
  b' ∷ BD.Block a → m r
  b' x@(BD.Paragraph is) = b x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  b' x@(BD.Header _ is) = b x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  b' x@(BD.Blockquote bs') = b x >>= \r → F.foldl (<>) r <$> T.traverse b' bs'
  b' x@(BD.Lst _ bss) = b x >>= \r → F.foldl (<>) r <<< join <$> T.traverse (\bs' → T.traverse b' bs') bss
  b' x = b x

  i' ∷ BD.Inline a → m r
  i' x@(BD.Emph is) = i x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  i' x@(BD.Strong is) = i x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  i' x@(BD.Link is _) = i x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  i' x@(BD.Image is _) = i x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  i' x = i x

everything
  ∷ ∀ r a
  . (Monoid r)
  ⇒ (BD.Block a → r)
  → (BD.Inline a → r)
  → BD.BloomDownP a
  → r
everything b i =
  un Id.Identity <<<
    everythingM
      (pure <<< b)
      (pure <<< i)
