module Text.Markdown.BloomDown.Pretty
  ( prettyPrintMd
  , prettyPrintTextBoxValue
  , prettyPrintBlock
  , prettyPrintInline
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (fold, elem)
import Data.Functor.Compose (Compose)
import Data.HugeNum as HN
import Data.Identity (Identity(..))
import Data.List as L
import Data.Maybe as M
import Data.Monoid (mempty)
import Data.String as S
import Data.Newtype (unwrap)
import Data.Unfoldable as U

import Text.Markdown.BloomDown.Syntax as BD

unlines ∷ L.List String → String
unlines lst = S.joinWith "\n" $ A.fromFoldable lst

prettyPrintMd ∷ ∀ a. (BD.Value a) ⇒ BD.BloomDownP a → String
prettyPrintMd (BD.BloomDown bs) = unlines $ L.concatMap prettyPrintBlock bs

replicateS ∷ Int → String → String
replicateS n s = fold (const s <$> (1 L... n))

indent ∷ Int → String → String
indent n s = replicateS n " " <> s

overLines ∷ (String → String) → L.List String → L.List String
overLines f = map f <<< L.concatMap lines

lines ∷ String → L.List String
lines "" = mempty
lines s = L.fromFoldable $ S.split (S.Pattern "\n") s

prettyPrintBlock ∷ ∀ a. (BD.Value a) ⇒ BD.Block a → L.List String
prettyPrintBlock bl =
  case bl of
    BD.Paragraph is → L.Cons (prettyPrintInlines is) (L.Cons "" L.Nil)
    BD.Header n is → L.singleton (replicateS n "#" <> " " <> prettyPrintInlines is)
    BD.Blockquote bs → overLines ((<>) "> ") (L.concatMap prettyPrintBlock bs)
    BD.Lst lt bss →
      let
        addMarker ∷ L.List String → L.List String
        addMarker L.Nil = L.Nil
        addMarker (L.Cons s ss) =
          let
            m = prettyPrintMarker lt
            len = S.length m
          in
            L.Cons (m <> " " <> s) $ overLines (indent (len + 1)) ss

        prettyPrintMarker ∷ BD.ListType → String
        prettyPrintMarker (BD.Bullet s) = s
        prettyPrintMarker (BD.Ordered s) = "1" <> s

        listItem ∷ L.List (BD.Block a) → L.List String
        listItem = addMarker <<< L.concatMap lines <<< L.concatMap prettyPrintBlock
      in
        L.concatMap listItem bss
    BD.CodeBlock ct ss →
      case ct of
        BD.Indented → indent 4 <$> ss
        BD.Fenced eval info →
          let
            bang
              | eval = "!"
              | otherwise = ""
          in
            L.singleton (bang <> "```" <> info) <> ss <> L.singleton "```"
    BD.LinkReference l url → L.singleton $ squares l <> ": " <> url
    BD.Rule → L.singleton "***"

prettyPrintInlines ∷ ∀ a. (BD.Value a) ⇒ L.List (BD.Inline a) → String
prettyPrintInlines is = S.joinWith "" $ A.fromFoldable $ (map prettyPrintInline is)

prettyPrintInline ∷ ∀ a. (BD.Value a) ⇒ BD.Inline a → String
prettyPrintInline il =
  case il of
    BD.Str s → s
    BD.Entity s → s
    BD.Space → " "
    BD.SoftBreak → "\n"
    BD.LineBreak → "  \n"
    BD.Emph is → "*" <> prettyPrintInlines is <> "*"
    BD.Strong is → "**" <> prettyPrintInlines is <> "**"
    BD.Code e s →
      let
        bang = if e then "!" else ""
      in
        bang <> "`" <> s <> "`"
    BD.Link is tgt → "[" <> prettyPrintInlines is <> "]" <> printTarget tgt
    BD.Image is url → "![" <> prettyPrintInlines is <> "](" <> url <> ")"
    BD.FormField l r e →
      let
        star = if r then "*" else" "
      in
        esc l <> star <> " = " <> prettyPrintFormElement e
      where

      esc s = M.maybe s (const $ "[" <> s <> "]") $ S.indexOf (S.Pattern " ") s

      printTarget ∷ BD.LinkTarget → String
      printTarget (BD.InlineLink url) = parens url
      printTarget (BD.ReferenceLink tgt) = squares (M.fromMaybe "" tgt)


prettyPrintTextBoxValue ∷ BD.TextBox Identity → String
prettyPrintTextBoxValue t =
  case t of
    BD.PlainText (Identity def) → def
    BD.Numeric (Identity def) →
      let s = HN.toString def in
      M.fromMaybe s $ S.stripSuffix (S.Pattern ".") $ HN.toString def
    BD.Date (Identity def) → prettyPrintDate def
    BD.Time prec (Identity def) → prettyPrintTime prec def
    BD.DateTime prec (Identity def) → prettyPrintDateTime prec def

prettyPrintDate ∷ BD.DateValue → String
prettyPrintDate { day, month, year } =
  printIntPadded 4 year
    <> "-"
    <> printIntPadded 2 month
    <> "-"
    <> printIntPadded 2 day

prettyPrintTime ∷ BD.TimePrecision → BD.TimeValue → String
prettyPrintTime prec { hours, minutes, seconds }=
  printIntPadded 2 hours
    <> ":"
    <> printIntPadded 2 minutes
    <> case prec of
        BD.Seconds -> ":" <> printIntPadded 2 (M.fromMaybe 0 seconds)
        _ -> ""

prettyPrintDateTime ∷ BD.TimePrecision → BD.DateTimeValue → String
prettyPrintDateTime prec { date, time } =
  prettyPrintDate date
    <> "T"
    <> prettyPrintTime prec time

printIntPadded ∷ Int → Int → String
printIntPadded l i =
  if dl > 0
  then S.fromCharArray (U.replicate dl '0') <> s
  else s
  where
    s = show i
    dl = l - S.length s

prettyPrintTextBox ∷ BD.TextBox (Compose M.Maybe BD.Expr) → String
prettyPrintTextBox t =
  prettyPrintTemplate t
    <> M.maybe "" (\x → " (" <> prettyPrintDefault x <> ")") (BD.traverseTextBox unwrap t)
  where
    prettyPrintTemplate ∷ ∀ f. BD.TextBox f → String
    prettyPrintTemplate =
      case _ of
        BD.PlainText _ → "______"
        BD.Numeric _ → "#______"
        BD.Date _ → "__-__-____"
        BD.Time BD.Minutes _ → "__:__"
        BD.Time BD.Seconds _ → "__:__:__"
        BD.DateTime BD.Minutes _ → "__-__-____ __:__"
        BD.DateTime BD.Seconds _ → "__-__-____ __:__:__"

    prettyPrintDefault ∷ BD.TextBox BD.Expr → String
    prettyPrintDefault =
      case _ of
        BD.PlainText def → prettyPrintExpr id id def
        BD.Numeric def → prettyPrintExpr id HN.toString def
        BD.Date def → prettyPrintExpr id prettyPrintDate def
        BD.Time prec def → prettyPrintExpr id (prettyPrintTime prec) def
        BD.DateTime prec def → prettyPrintExpr id (prettyPrintDateTime prec) def


prettyPrintFormElement ∷ ∀ a. (BD.Value a) ⇒ BD.FormField a → String
prettyPrintFormElement el =
  case el of
    BD.TextBox tb → prettyPrintTextBox tb
    BD.RadioButtons (BD.Literal sel) (BD.Literal ls) →
      let
        radioButton l = (if l == sel then "(x) " else "() ") <> BD.renderValue l
      in
        S.joinWith " " $ A.fromFoldable (map radioButton ls)
    BD.RadioButtons (BD.Unevaluated bs) (BD.Unevaluated ls) →
      "(!`" <> bs <> "`) !`" <> ls <> "`"
    BD.CheckBoxes (BD.Literal sel) (BD.Literal ls) →
      let
        checkBox l = (if elem l sel then "[x] " else "[] ") <> BD.renderValue l
      in
        S.joinWith " " <<< A.fromFoldable $ checkBox <$> ls
    BD.CheckBoxes (BD.Unevaluated bs) (BD.Unevaluated ls) →
      "[!`" <> bs <> "`] !`" <> ls <> "`"
    BD.DropDown sel lbls →
      braces (prettyPrintExpr id (A.fromFoldable >>> map BD.renderValue >>> S.joinWith ", ") lbls)
        <> M.maybe "" (parens <<< prettyPrintExpr id BD.renderValue) sel
    _ → "Unsupported form element"

prettyPrintExpr ∷ ∀ a. (String → String) → (a → String) → BD.Expr a → String
prettyPrintExpr _ f (BD.Literal a) = f a
prettyPrintExpr wrap _ (BD.Unevaluated c) = wrap $ "!`" <> c <> "`"

parens ∷ String → String
parens s = "(" <> s <> ")"

braces ∷ String → String
braces s = "{" <> s <> "}"

squares ∷ String → String
squares s = "[" <> s <> "]"
