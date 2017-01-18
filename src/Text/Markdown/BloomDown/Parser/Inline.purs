module Text.Markdown.BloomDown.Parser.Inline
  ( parseInlines
  , validateFormField
  , validateInline
  , parseTextBox
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy as Lazy

import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Functor.Compose (Compose(..))
import Data.HugeNum as HN
import Data.Int as Int
import Data.List as L
import Data.Maybe as M
import Data.String as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Validation.Semigroup as V

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

import Text.Markdown.BloomDown.Parser.Utils as PU
import Text.Markdown.BloomDown.Syntax as BD

parseInlines
  ∷ ∀ a
  . (BD.Value a)
  ⇒ L.List String
  → Either String (L.List (BD.Inline a))
parseInlines s =
  map consolidate
    $ lmap P.parseErrorMessage
    $ P.runParser (S.joinWith "\n" $ A.fromFoldable s) inlines

consolidate
  ∷ ∀ a
  . L.List (BD.Inline a)
  → L.List (BD.Inline a)
consolidate xs =
  case xs of
    L.Nil → L.Nil
    L.Cons (BD.Str s1) (L.Cons (BD.Str s2) is) →
      consolidate $ L.Cons (BD.Str (s1 <> s2)) is
    L.Cons i is → L.Cons i $ consolidate is

someOf
  ∷ (Char → Boolean)
  → P.Parser String String
someOf =
  map (S.fromCharArray <<< A.fromFoldable)
    <<< L.some
    <<< PS.satisfy

manyOf
  ∷ (Char → Boolean)
  → P.Parser String String
manyOf =
  map (S.fromCharArray <<< A.fromFoldable)
    <<< L.many
    <<< PS.satisfy

isNumeric ∷ Char → Boolean
isNumeric c =
  s >= "0" && s <= "9"
  where
    s = S.singleton c

dash ∷ P.Parser String Unit
dash = void $ PS.string "-"

colon ∷ P.Parser String Unit
colon = void $ PS.string ":"

dot ∷ P.Parser String Unit
dot = void $ PS.string "."

hash ∷ P.Parser String Unit
hash = void $ PS.string "#"

type TextParserKit
  = { plainText ∷ P.Parser String String
    , natural ∷ P.Parser String Int
    , decimal ∷ P.Parser String HN.HugeNum
    , numericPrefix ∷ P.Parser String Unit
    }

validateFormField
  ∷ ∀ a
  . BD.FormField a
  → V.V (Array String) (BD.FormField a)
validateFormField field =
  case field of
    BD.CheckBoxes (BD.Literal _) (BD.Unevaluated _) →
      V.invalid ["Checkbox values & selection must be both literals or both unevaluated expressions"]
    BD.CheckBoxes (BD.Unevaluated _) (BD.Literal _) →
      V.invalid ["Checkbox values & selection must be both literals or both unevaluated expressions"]
    _ → pure field

validateInline
  ∷ ∀ a
  . BD.Inline a
  → V.V (Array String) (BD.Inline a)
validateInline inl =
  case inl of
    BD.Emph inls → BD.Emph <$> traverse validateInline inls
    BD.Strong inls → BD.Strong <$> traverse validateInline inls
    BD.Link inls targ → BD.Link <$> traverse validateInline inls <*> pure targ
    BD.Image inls str → BD.Image <$> traverse validateInline inls <*> pure str
    BD.FormField str b ff → BD.FormField str b <$> validateFormField ff
    _ → pure inl


inlines
  ∷ ∀ a
  . (BD.Value a)
  ⇒ P.Parser String (L.List (BD.Inline a))
inlines = L.many inline2 <* PS.eof
  where
  inline0 ∷ P.Parser String (BD.Inline a)
  inline0 =
    Lazy.fix \p →
      alphaNumStr
        <|> space
        <|> strongEmph p
        <|> strong p
        <|> emph p
        <|> code
        <|> autolink
        <|> entity

  inline1 ∷ P.Parser String (BD.Inline a)
  inline1 =
    PC.try inline0
      <|> PC.try link

  inline2 ∷ P.Parser String (BD.Inline a)
  inline2 = do
    res ←
      PC.try formField
      <|> PC.try (Right <$> inline1)
      <|> PC.try (Right <$> image)
      <|> (Right <$> other)
    case res of
      Right v → pure v
      Left e → P.fail e

  alphaNumStr ∷ P.Parser String (BD.Inline a)
  alphaNumStr = BD.Str <$> someOf isAlphaNum

  isAlphaNum ∷ Char → Boolean
  isAlphaNum c =
    (s >= "a" && s <= "z") ||
    (s >= "A" && s <= "Z") ||
    (s >= "0" && s <= "9")
    where s = S.singleton c

  emphasis
    ∷ P.Parser String (BD.Inline a)
    → (L.List (BD.Inline a) → BD.Inline a)
    → String
    → P.Parser String (BD.Inline a)
  emphasis p f s = do
    PS.string s
    f <$> PC.manyTill p (PS.string s)

  emph ∷ P.Parser String (BD.Inline a) → P.Parser String (BD.Inline a)
  emph p = emphasis p BD.Emph "*" <|> emphasis p BD.Emph "_"

  strong ∷ P.Parser String (BD.Inline a) → P.Parser String (BD.Inline a)
  strong p = emphasis p BD.Strong "**" <|> emphasis p BD.Strong "__"

  strongEmph ∷ P.Parser String (BD.Inline a) → P.Parser String (BD.Inline a)
  strongEmph p = emphasis p f "***" <|> emphasis p f "___"
    where
    f is = BD.Strong $ L.singleton $ BD.Emph is

  space ∷ P.Parser String (BD.Inline a)
  space = (toSpace <<< (S.singleton <$> _)) <$> L.some (PS.satisfy PU.isWhitespace)
    where
    toSpace cs
      | "\n" `elem` cs =
        case L.take 2 cs of
          L.Cons " " (L.Cons " " L.Nil) → BD.LineBreak
          _ → BD.SoftBreak
      | otherwise = BD.Space

  code ∷ P.Parser String (BD.Inline a)
  code = do
    eval ← PC.option false (PS.string "!" *> pure true)
    ticks ← someOf (\x → S.singleton x == "`")
    contents ← (S.fromCharArray <<< A.fromFoldable) <$> PC.manyTill PS.anyChar (PS.string ticks)
    pure <<< BD.Code eval <<< S.trim $ contents


  link ∷ P.Parser String (BD.Inline a)
  link = BD.Link <$> linkLabel <*> linkTarget
    where
    linkLabel ∷ P.Parser String (L.List (BD.Inline a))
    linkLabel = PS.string "[" *> PC.manyTill (inline0 <|> other) (PS.string "]")

    linkTarget ∷ P.Parser String BD.LinkTarget
    linkTarget = inlineLink <|> referenceLink

    inlineLink ∷ P.Parser String BD.LinkTarget
    inlineLink = BD.InlineLink <<< S.fromCharArray <<< A.fromFoldable <$> (PS.string "(" *> PC.manyTill PS.anyChar (PS.string ")"))

    referenceLink ∷ P.Parser String BD.LinkTarget
    referenceLink = BD.ReferenceLink <$> PC.optionMaybe ((S.fromCharArray <<< A.fromFoldable) <$> (PS.string "[" *> PC.manyTill PS.anyChar (PS.string "]")))

  image ∷ P.Parser String (BD.Inline a)
  image = BD.Image <$> imageLabel <*> imageUrl
    where
    imageLabel ∷ P.Parser String (L.List (BD.Inline a))
    imageLabel = PS.string "![" *> PC.manyTill (inline1 <|> other) (PS.string "]")

    imageUrl ∷ P.Parser String String
    imageUrl = S.fromCharArray <<< A.fromFoldable <$> (PS.string "(" *> PC.manyTill PS.anyChar (PS.string ")"))

  autolink ∷ P.Parser String (BD.Inline a)
  autolink = do
    PS.string "<"
    url ← (S.fromCharArray <<< A.fromFoldable) <$> (PS.anyChar `PC.many1Till` PS.string ">")
    pure $ BD.Link (L.singleton $ BD.Str (autoLabel url)) (BD.InlineLink url)
    where
    autoLabel ∷ String → String
    autoLabel s
      | PU.isEmailAddress s = "mailto:" <> s
      | otherwise = s

  entity ∷ P.Parser String (BD.Inline a)
  entity = do
    PS.string "&"
    s ← (S.fromCharArray <<< A.fromFoldable) <$> (PS.noneOf (S.toCharArray ";") `PC.many1Till` PS.string ";")
    pure $ BD.Entity $ "&" <> s <> ";"

  formField ∷ P.Parser String (Either String (BD.Inline a))
  formField =
    do
      l ← label
      r ← do
          PU.skipSpaces
          required
      fe ← do
        PU.skipSpaces
        PS.string "="
        PU.skipSpaces
        formElement
      pure $ map (BD.FormField l r) fe
    where
    label =
      someOf isAlphaNum
      <|> (S.fromCharArray
             <<< A.fromFoldable
             <$> (PS.string "[" *> PC.manyTill PS.anyChar (PS.string "]")))

    required = PC.option false (PS.string "*" *> pure true)

  formElement ∷ P.Parser String (Either String (BD.FormField a))
  formElement =
    PC.try textBox
      <|> PC.try (Right <$> radioButtons)
      <|> PC.try (Right <$> checkBoxes)
      <|> PC.try (Right <$> dropDown)
    where

    textBox ∷ P.Parser String (Either String (BD.FormField a))
    textBox = do
      template ← parseTextBoxTemplate
      PU.skipSpaces
      defVal ← PC.optionMaybe $ PS.string "("
      case defVal of
        M.Nothing → pure $ Right $ BD.TextBox $ BD.transTextBox (const $ Compose M.Nothing) template
        M.Just _ → do
          PU.skipSpaces
          mdef ← PC.optionMaybe $ PC.try $ parseTextBox (_ /= ')') (expr id) template
          case mdef of
            M.Just def → do
              PU.skipSpaces
              PS.string ")"
              pure $ Right $ BD.TextBox $ BD.transTextBox (M.Just >>> Compose) def
            M.Nothing →
              pure $ Left case template of
                BD.DateTime BD.Minutes _ →
                  "Incorrect datetime default value, please use \"YYYY-MM-DD HH:mm\" or \"YYYY-MM-DDTHH:mm\" format"
                BD.DateTime BD.Seconds _ →
                  "Incorrect datetime default value, please use \"YYYY-MM-DD HH:mm:ss\" or \"YYYY-MM-DDTHH:mm:ss\" format"
                BD.Date _ →
                  "Incorrect date default value, please use \"YYYY-MM-DD\" format"
                BD.Time BD.Minutes _ →
                  "Incorrect time default value, please use \"HH:mm\" format"
                BD.Time BD.Seconds _ →
                  "Incorrect time default value, please use \"HH:mm:ss\" format"
                BD.Numeric _ →
                  "Incorrect numeric default value"
                BD.PlainText _ →
                  "Incorrect default value"

    parseTextBoxTemplate ∷ P.Parser String (BD.TextBox (Const Unit))
    parseTextBoxTemplate =
      BD.DateTime BD.Seconds (Const unit) <$ PC.try (parseDateTimeTemplate BD.Seconds)
        <|> BD.DateTime BD.Minutes (Const unit) <$ PC.try (parseDateTimeTemplate BD.Minutes)
        <|> BD.Date (Const unit) <$ PC.try parseDateTemplate
        <|> BD.Time BD.Seconds (Const unit) <$ PC.try (parseTimeTemplate BD.Seconds)
        <|> BD.Time BD.Minutes (Const unit) <$ PC.try (parseTimeTemplate BD.Minutes)
        <|> BD.Numeric (Const unit) <$ PC.try parseNumericTemplate
        <|> BD.PlainText (Const unit) <$ parsePlainTextTemplate

      where
        parseDateTimeTemplate prec = do
          parseDateTemplate
          PU.skipSpaces
          parseTimeTemplate prec

        parseDateTemplate = do
          und
          PU.skipSpaces *> dash *> PU.skipSpaces
          und
          PU.skipSpaces *> dash *> PU.skipSpaces
          und

        parseTimeTemplate prec = do
          und
          PU.skipSpaces *> colon *> PU.skipSpaces
          und
          when (prec == BD.Seconds) do
            PU.skipSpaces *> colon *> PU.skipSpaces
            void und

        parseNumericTemplate = do
          hash
          und

        parsePlainTextTemplate =
          und


    und ∷ P.Parser String String
    und = someOf (\x → x == '_')

    radioButtons ∷ P.Parser String (BD.FormField a)
    radioButtons = literalRadioButtons <|> evaluatedRadioButtons
      where
        literalRadioButtons = do
          ls ← L.some $ PC.try do
            let item = BD.stringValue <<< S.trim <$> manyOf \c → not $ c `elem` ['(',')','!','`']
            PU.skipSpaces
            b ← (PS.string "(x)" *> pure true) <|> (PS.string "()" *> pure false)
            PU.skipSpaces
            l ← item
            pure $ Tuple b l
          sel ←
            case L.filter fst ls of
              L.Cons (Tuple _ l) L.Nil → pure l
              _ → P.fail "Invalid number of selected radio buttons"
          pure $ BD.RadioButtons (BD.Literal sel) (BD.Literal (map snd ls))

        evaluatedRadioButtons = do
          BD.RadioButtons
            <$> PU.parens unevaluated
            <*> (PU.skipSpaces *> unevaluated)

    checkBoxes ∷ P.Parser String (BD.FormField a)
    checkBoxes = literalCheckBoxes <|> evaluatedCheckBoxes
      where
        literalCheckBoxes = do
          ls ← L.some $ PC.try do
            let item = BD.stringValue <<< S.trim <$> manyOf \c → not $ c `elem` ['[',']','!','`']
            PU.skipSpaces
            b ← (PS.string "[x]" *> pure true) <|> (PS.string "[]" *> pure false)
            PU.skipSpaces
            l ← item
            pure $ Tuple b l
          pure $ BD.CheckBoxes (BD.Literal $ snd <$> L.filter fst ls) (BD.Literal $ snd <$> ls)

        evaluatedCheckBoxes =
          BD.CheckBoxes
            <$> PU.squares unevaluated
            <*> (PU.skipSpaces *> unevaluated)

    dropDown ∷ P.Parser String (BD.FormField a)
    dropDown = do
      let item = BD.stringValue <<< S.trim <$> manyOf \c → not $ c `elem` ['{','}',',','!','`','(',')']
      ls ← PU.braces $ expr id $ (PC.try (PU.skipSpaces *> item)) `PC.sepBy` (PU.skipSpaces *> PS.string ",")
      sel ← PC.optionMaybe $ PU.skipSpaces *> (PU.parens $ expr id $ item)
      pure $ BD.DropDown sel ls

  other ∷ P.Parser String (BD.Inline a)
  other = do
    c ← S.singleton <$> PS.anyChar
    if c == "\\"
      then
        (BD.Str <<< S.singleton) <$> PS.anyChar
          <|> (PS.satisfy (\x → S.singleton x == "\n") *> pure BD.LineBreak)
          <|> pure (BD.Str "\\")
      else pure (BD.Str c)

parseTextBox
  ∷ ∀ f g
  . (Char → Boolean)
  → (∀ a. P.Parser String a → P.Parser String (g a))
  → BD.TextBox f
  → P.Parser String (BD.TextBox g)
parseTextBox isPlainText eta template =
  case template of
    BD.DateTime prec _ → BD.DateTime prec <$> eta (parseDateTimeValue prec)
    BD.Date _ → BD.Date <$> eta parseDateValue
    BD.Time prec _ → BD.Time prec <$> eta (parseTimeValue prec)
    BD.Numeric _ → BD.Numeric <$> eta parseNumericValue
    BD.PlainText _ → BD.PlainText <$> eta parsePlainTextValue

  where
    parseDateTimeValue prec = do
      date ← parseDateValue
      (PC.try $ void $ PS.string "T") <|> PU.skipSpaces
      time ← parseTimeValue prec
      pure { date, time }

    parseDateValue = do
      year ← parseYear
      PU.skipSpaces *> dash *> PU.skipSpaces
      month ← natural
      when (month > 12) $ P.fail "Incorrect month"
      PU.skipSpaces *> dash *> PU.skipSpaces
      day ← natural
      when (day > 31) $ P.fail "Incorrect day"
      pure { month, day, year }

    parseTimeValue prec = do
      hours ← natural
      when (hours > 23) $ P.fail "Incorrect hours"
      PU.skipSpaces *> colon *> PU.skipSpaces
      minutes ← natural
      when (minutes > 59) $ P.fail "Incorrect minutes"
      seconds ← case prec of
        BD.Minutes -> do
          scolon ← PC.try $ PC.optionMaybe $ PU.skipSpaces *> colon
          when (M.isJust scolon) $ P.fail "Unexpected seconds component"
          pure M.Nothing
        BD.Seconds -> do
          PU.skipSpaces *> colon *> PU.skipSpaces
          secs ← natural
          when (secs > 59) $ P.fail "Incorrect seconds"
          PU.skipSpaces
          pure $ M.Just secs
      PU.skipSpaces
      amPm ←
        PC.optionMaybe $
          (false <$ (PS.string "PM" <|> PS.string "pm"))
            <|> (true <$ (PS.string "AM" <|> PS.string "am"))
      let hours' =
            case amPm of
              M.Nothing → hours
              M.Just isAM →
                if not isAM && hours < 12
                then hours + 12
                else if isAM && hours == 12
                then 0
                else hours
      pure { hours : hours', minutes, seconds }

    parseNumericValue = do
      sign ← PC.try (-1 <$ PS.char '-') <|> pure 1
      ms ← digits
      PU.skipSpaces
      gotDot ← PC.optionMaybe dot
      PU.skipSpaces

      ns ←
        case gotDot of
          M.Just _ → do
            PC.optionMaybe (PU.skipSpaces *> digits)
          M.Nothing →
            pure M.Nothing

      HN.fromString (ms <> "." <> M.fromMaybe "" ns)
        # M.maybe (P.fail "Failed parsing decimal") pure

    parsePlainTextValue =
      manyOf isPlainText

    natural = do
      xs ← digits
      Int.fromString xs
        # M.maybe (P.fail "Failed parsing natural") pure

    digit =
      PS.oneOf ['0','1','2','3','4','5','6','7','8','9']

    digitN = do
      ds ← digit
      ds
        # pure
        # S.fromCharArray
        # Int.fromString
        # M.maybe (P.fail "Failed parsing digit") pure

    parseYear = do
      millenia ← digitN
      centuries ← digitN
      decades ← digitN
      years ← digitN
      pure $ 1000 * millenia + 100 * centuries + 10 * decades + years

    digits =
      L.some digit <#>
        A.fromFoldable >>> S.fromCharArray

expr
  ∷ ∀ b
  . (∀ e. P.Parser String e → P.Parser String e)
  → P.Parser String b
  → P.Parser String (BD.Expr b)
expr f p =
  PC.try (f unevaluated)
    <|> BD.Literal <$> p

unevaluated ∷ ∀ b. P.Parser String (BD.Expr b)
unevaluated = do
  PS.string "!"
  ticks ← someOf (\x → S.singleton x == "`")
  BD.Unevaluated <<< S.fromCharArray <<< A.fromFoldable <$> PC.manyTill PS.anyChar (PS.string ticks)
