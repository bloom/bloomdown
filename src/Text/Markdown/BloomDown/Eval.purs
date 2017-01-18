module Text.Markdown.BloomDown.Eval
  ( eval
  , LanguageId
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.Array as A
import Data.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.List as L
import Data.Maybe as M
import Data.Newtype (unwrap)
import Data.String as S
import Data.Traversable as T

import Text.Markdown.BloomDown.Syntax as BD
import Text.Markdown.BloomDown.Traverse (everywhereM)

type LanguageId = String

eval
  ∷ ∀ m a
  . (Monad m, BD.Value a)
  ⇒ { code ∷ M.Maybe LanguageId → String → m a
    , textBox ∷ BD.TextBox (Const String) → m (BD.TextBox Identity)
    , value ∷ String → m a
    , list ∷ String → m (L.List a)
    }
  → BD.BloomDownP a
  → m (BD.BloomDownP a)
eval fs = everywhereM b i
  where

  b ∷ BD.Block a → m (BD.Block a)
  b (BD.CodeBlock (BD.Fenced true info) code) =
    BD.CodeBlock (BD.Fenced false info) <<< pure <<< BD.renderValue
      <$> fs.code (M.Just info) (S.joinWith "\n" (A.fromFoldable code))
  b other = pure $ other

  i ∷ BD.Inline a → m (BD.Inline a)
  i (BD.Code true code) = BD.Code false <<< BD.renderValue <$> fs.code M.Nothing code
  i (BD.FormField l r field) = BD.FormField l r <$> f field
  i other = pure $ other

  f ∷ BD.FormField a → m (BD.FormField a)
  f (BD.TextBox tb) = BD.TextBox <<< M.fromMaybe tb <$> nbeTextBox tb
    where
      -- normalization-by-evaluation proceeds by evaluating an object into a semantic model
      -- (in this case, `Identity`), and then quoting the result back into the syntax.
      nbeTextBox ∷ BD.TextBox (Compose M.Maybe BD.Expr) → m (M.Maybe (BD.TextBox (Compose M.Maybe BD.Expr)))
      nbeTextBox = evalTextBox >>> map (map quoteTextBox)

      evalTextBox ∷ BD.TextBox (Compose M.Maybe BD.Expr) → m (M.Maybe (BD.TextBox Identity))
      evalTextBox tb' = T.sequence $ fs.textBox <$> asCode tb' <|> pure <$> asLit tb'
        where
          asLit = BD.traverseTextBox (unwrap >>> (_ >>= BD.getLiteral) >>> map Identity)
          asCode = BD.traverseTextBox (unwrap >>> (_ >>= BD.getUnevaluated) >>> map Const)

      quoteTextBox ∷ BD.TextBox Identity → BD.TextBox (Compose M.Maybe BD.Expr)
      quoteTextBox = BD.transTextBox (unwrap >>> BD.Literal >>> M.Just >>> Compose)

  f (BD.RadioButtons sel opts) = do
    sel' ← evalExpr fs.value sel
    opts' ← evalExpr fs.list opts
    pure $ BD.RadioButtons sel' (mergeSelection (L.singleton <$> sel') opts')

  f (BD.CheckBoxes sel vals) = do
    sel' ← evalExpr fs.list sel
    vals' ← evalExpr fs.list vals
    pure $ BD.CheckBoxes sel' (mergeSelection sel' vals')

  f (BD.DropDown msel opts) = do
    msel' ← T.traverse (evalExpr fs.value) msel
    opts' ← evalExpr fs.list opts
    pure $ BD.DropDown msel' $ M.maybe opts' (flip mergeSelection opts' <<< map L.singleton) msel'

  mergeSelection ∷ BD.Expr (L.List a) → BD.Expr (L.List a) → BD.Expr (L.List a)
  mergeSelection (BD.Literal sel) (BD.Literal xs) = BD.Literal $ L.union sel xs
  mergeSelection _ exs = exs

  evalExpr ∷ ∀ e. (String → m e) → BD.Expr e → m (BD.Expr e)
  evalExpr _ (BD.Literal a) = pure $ BD.Literal a
  evalExpr e (BD.Unevaluated s) = BD.Literal <$> e s

  getValues ∷ ∀ e. BD.Expr (L.List e) → L.List e
  getValues (BD.Literal vs) = vs
  getValues _ = L.Nil
