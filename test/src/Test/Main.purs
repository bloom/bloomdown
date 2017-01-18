module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C
import Control.Monad.Eff.Random as Rand
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Trampoline as Trampoline

import Data.HugeNum as HN
import Data.Either (Either(..), isLeft)
import Data.List as L
import Data.Maybe as M
import Data.Newtype (un)
import Data.Traversable as TR
import Data.Identity as ID
import Data.Array as A
import Data.Char as CH
import Data.String as S

import Data.Tuple (uncurry)

import Text.Markdown.BloomDown.Syntax as BD
import Text.Markdown.BloomDown.Eval as BDE
import Text.Markdown.BloomDown.Parser as BDP
import Text.Markdown.BloomDown.Pretty as BDPR

import Test.StrongCheck as SC
import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.LCG as LCG

type TestEffects e =
  ( console ∷ C.CONSOLE
  , random ∷ Rand.RANDOM
  , err ∷ Exn.EXCEPTION
  | e
  )

newtype NonEmptyString = NonEmptyString String
derive instance eqNonEmptyString ∷ Eq NonEmptyString
derive instance ordNonEmptyString ∷ Ord NonEmptyString

genChar ∷ Gen.Gen Char
genChar = Gen.elements '-' $ L.fromFoldable $ S.toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 @!#$%^"

instance arbitraryNonEmptyString ∷ SCA.Arbitrary NonEmptyString where
  arbitrary =
    Gen.arrayOf1 genChar
      <#> uncurry A.cons
      >>> S.fromCharArray
      >>> S.trim
      >>> NonEmptyString

instance showNonEmptyString ∷ Show NonEmptyString where
  show (NonEmptyString str) = str

instance valueNonEmptyString ∷ BD.Value NonEmptyString where
  stringValue = NonEmptyString
  renderValue (NonEmptyString str) = str

testDocument ∷ ∀ e. Either String (BD.BloomDownP NonEmptyString) → Eff (TestEffects e) Unit
testDocument BD = do
  let printed = BDPR.prettyPrintMd <$> BD
      parsed = printed >>= BDP.parseMd

  C.log
    $ "Original: \n   "
    <> show BD
    <> "\nPrinted:\n   "
    <> show printed
    <> "\nParsed:\n   "
    <> show parsed
  SC.assert (parsed == BD SC.<?> "Test failed")

failDocument ∷ ∀ e. Either String (BD.BloomDownP NonEmptyString) → Eff (TestEffects e) Unit
failDocument BD = SC.assert (isLeft BD SC.<?> "Test failed")

static ∷ ∀ e. Eff (TestEffects e) Unit
static = do
  testDocument $ BDP.parseMd "Paragraph"
  testDocument $ BDP.parseMd "Paragraph with spaces"
  testDocument $ BDP.parseMd "Paragraph with an entity: &copy;"
  testDocument $ BDP.parseMd "Paragraph with a [link](http://purescript.org)"
  testDocument $ BDP.parseMd "Paragraph with an ![image](image.png)"
  testDocument $ BDP.parseMd "Paragraph with some `embedded code`"
  testDocument $ BDP.parseMd "Paragraph with some !`code which can be evaluated`"
  testDocument $ BDP.parseMd "Paragraph with _emphasis_"
  testDocument $ BDP.parseMd "Paragraph with _emphasis_ and __strong text__"

  testDocument $
    BDP.parseMd
      "Paragraph with a\n\
      \soft break"

  testDocument $
    BDP.parseMd
      "Paragraph with a  \n\
      \line break"

  testDocument $
    BDP.parseMd
      "Two\n\
      \\n\
      \paragraphs"

  testDocument $
    BDP.parseMd
      "Header\n\
      \==="

  testDocument $
    BDP.parseMd
      "# Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    BDP.parseMd
      "## Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    BDP.parseMd
      "### Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    BDP.parseMd
      "#### Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    BDP.parseMd
      "##### Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    BDP.parseMd
      "###### Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    BDP.parseMd
      "Rule:\n\
      \\n\
      \-----"

  testDocument $
    BDP.parseMd
      "A blockquote:\n\
      \\n\
      \> Here is some text\n\
      \> inside a blockquote"

  testDocument $
    BDP.parseMd
      "A nested blockquote:\n\
      \\n\
      \> Here is some text\n\
      \> > Here is some more text"

  testDocument $
    BDP.parseMd
      "An unordered list:\n\
      \\n\
      \* Item 1\n\
      \* Item 2"

  testDocument $
    BDP.parseMd
      "An ordered list:\n\
      \\n\
      \1. Item 1\n\
      \1. Item 2"

  testDocument $
    BDP.parseMd
      "A nested list:\n\
      \\n\
      \1. Item 1\n\
      \1. 1. Item 2\n\
      \   1. Item 3"

  testDocument $
    BDP.parseMd
      "Some indented code:\n\
      \\n\
      \    import Debug.Log\n\
      \    \n\
      \    main = log \"Hello World\""

  testDocument $
    BDP.parseMd
      "Some fenced code:\n\
      \\n\
      \```purescript\n\
      \import Debug.Log\n\
      \\n\
      \main = log \"Hello World\"\n\
      \```"

  testDocument $
    BDP.parseMd
      "Some fenced code which can be evaluated:\n\
      \\n\
      \!~~~purescript\n\
      \import Debug.Log\n\
      \\n\
      \main = log \"Hello World\"\n\
      \~~~"

  let
    probablyParsedCodeForEvaluation =
      BDP.parseMd
        "Some evaluated fenced code:\n\
        \\n\
        \!~~~purescript\n\
        \import Debug.Log\n\
        \\n\
        \main = log \"Hello World\"\n\
        \~~~"

  testDocument
    case probablyParsedCodeForEvaluation of
      Right BD →
        Right
          $ un ID.Identity
          $ BDE.eval
            { code: \_ _ → pure $ BD.stringValue "Evaluated code block!"
            , textBox: \t →
                case t of
                  BD.PlainText _ → pure $ BD.PlainText $ pure "Evaluated plain text!"
                  BD.Numeric _ → pure $ BD.Numeric $ pure $ HN.fromNumber 42.0
                  BD.Date _ → pure $ BD.Date $ pure { month : 7, day : 30, year : 1992 }
                  BD.Time (prec@BD.Minutes) _ → pure $ BD.Time prec $ pure { hours : 4, minutes : 52, seconds : M.Nothing }
                  BD.Time (prec@BD.Seconds) _ → pure $ BD.Time prec $ pure { hours : 4, minutes : 52, seconds : M.Just 10 }
                  BD.DateTime (prec@BD.Minutes) _ →
                    pure $ BD.DateTime prec $ pure $
                      { date : { month : 7, day : 30, year : 1992 }
                      , time : { hours : 4, minutes : 52, seconds : M.Nothing }
                      }
                  BD.DateTime (prec@BD.Seconds) _ →
                    pure $ BD.DateTime prec $ pure $
                      { date : { month : 7, day : 30, year : 1992 }
                      , time : { hours : 4, minutes : 52, seconds : M.Just 10 }
                      }
            , value: \_ → pure $ BD.stringValue "Evaluated value!"
            , list: \_ → pure $ L.singleton $ BD.stringValue "Evaluated list!"
            }  BD
      a → a

  testDocument $ BDP.parseMd "name = __ (Phil Freeman)"
  testDocument $ BDP.parseMd "name = __ (!`name`)"
  testDocument $ BDP.parseMd "sex* = (x) male () female () other"
  testDocument $ BDP.parseMd "sex* = (!`def`) !`others`"
  testDocument $ BDP.parseMd "city = {BOS, SFO, NYC} (NYC)"
  testDocument $ BDP.parseMd "city = {!`...`} (!`...`)"
  testDocument $ BDP.parseMd "phones = [] Android [x] iPhone [x] Blackberry"
  testDocument $ BDP.parseMd "phones = [!`...`] !`...`"
  testDocument $ BDP.parseMd "start = __ - __ - ____ (06-06-2015)"
  testDocument $ BDP.parseMd "start = __ - __ - ____ (!`...`)"
  testDocument $ BDP.parseMd "start = __ : __ (10:32 PM)"
  failDocument $ BDP.parseMd "start = __ : __ (10:32:46 PM)"
  failDocument $ BDP.parseMd "start = __ : __ : __ (10:32 PM)"
  testDocument $ BDP.parseMd "start = __ : __ : __ (10:32:46 PM)"
  testDocument $ BDP.parseMd "start = __ : __ (!`...`)"
  testDocument $ BDP.parseMd "start = __-__-____ __:__ (06-06-2015 12:00 PM)"
  testDocument $ BDP.parseMd "start = __ - __ - ____ __ : __ (!`...`)"
  testDocument $ BDP.parseMd "[zip code]* = __ (12345)"
  testDocument $ BDP.parseMd "defaultless = __"
  testDocument $ BDP.parseMd "city = {BOS, SFO, NYC}"
  testDocument $ BDP.parseMd "start = __ - __ - ____"
  testDocument $ BDP.parseMd "start = __ : __"
  testDocument $ BDP.parseMd "start = __ : __ : __"
  testDocument $ BDP.parseMd "start = __ - __ - ____ __ : __ : __"
  testDocument $ BDP.parseMd "zip* = ________"
  testDocument $ BDP.parseMd "[numeric field] = #______ (23)"
  testDocument $ BDP.parseMd "i9a0qvg8* = ______ (9a0qvg8h)"
  testDocument $ BDP.parseMd "xeiodbdy  = [x] "

  C.log "All static tests passed!"

generated ∷ ∀ e. Eff (TestEffects e) Unit
generated = do
  C.log "Random documents"
  seed ← LCG.randomSeed
  let
    docs =
      Trampoline.runTrampoline
        $ Gen.sample'
            10 (Gen.GenState { size: 10, seed })
            (BDPR.prettyPrintMd <<< runTestBloomDown <$> SCA.arbitrary)

  TR.traverse C.log docs

  SC.quickCheck' 100 \(TestBloomDown BD) →
    let
      printed = BDPR.prettyPrintMd BD
      parsed = BDP.parseMd printed
    in parsed == (Right BD) SC.<?> "Pretty printer and parser incompatible for document: " <>
      "\nOriginal: \n" <> show BD <>
      "\nPrinted: \n" <> printed <>
      "\nParsed: \n" <> show parsed
  C.log "All dynamic passed"

deferGen ∷ ∀ a. (Unit → Gen.Gen a) → Gen.Gen a
deferGen g = do
  u ← pure unit
  g u

tinyArrayOf ∷ ∀ a. Gen.Gen a → Gen.Gen (Array a)
tinyArrayOf g = do
  len ← Gen.chooseInt 0 1
  Gen.vectorOf len g

smallArrayOf ∷ ∀ a. Gen.Gen a → Gen.Gen (Array a)
smallArrayOf g = do
  len ← Gen.chooseInt 1 2
  Gen.vectorOf len g

newtype TestBloomDown = TestBloomDown (BD.BloomDownP NonEmptyString)

runTestBloomDown ∷ TestBloomDown → BD.BloomDownP NonEmptyString
runTestBloomDown (TestBloomDown BD) = BD

instance arbBloomDown ∷ SCA.Arbitrary TestBloomDown where
  arbitrary = (TestBloomDown <<< BD.BloomDown <<< L.fromFoldable) <$> blocks

three ∷ ∀ a. a → a → a → Array a
three a b c = [a, b, c]


blocks ∷ ∀ a. (SCA.Arbitrary a, BD.Value a) ⇒ Gen.Gen (Array (BD.Block a))
blocks =
  Gen.oneOf (smallArrayOf block0)
    [ A.singleton <$> bq
    , A.singleton <$> list
    , A.singleton <$> cb
    ]
  where
  block0 ∷ Gen.Gen (BD.Block a)
  block0 =
    Gen.oneOf (BD.Paragraph <<< L.fromFoldable <$> inlines)
      [ BD.Header <$> Gen.chooseInt 1 6 <*> (L.singleton <$> simpleText)
      , BD.CodeBlock <$>
        (BD.Fenced <$> (Gen.elements true (L.singleton false)) <*>
         alphaNum)
        <*> (L.fromFoldable <$> smallArrayOf alphaNum)
      , BD.LinkReference <$> alphaNum <*> alphaNum
      , pure BD.Rule
      ]

  bq ∷ Gen.Gen (BD.Block a)
  bq = BD.Blockquote <$> (L.singleton <$> block0)

  cb ∷ Gen.Gen (BD.Block a)
  cb = BD.CodeBlock BD.Indented <<< L.fromFoldable <$> smallArrayOf alphaNum

  list ∷ Gen.Gen (BD.Block a)
  list =
    BD.Lst
      <$> Gen.oneOf (BD.Bullet <$> (Gen.elements "-" $ L.fromFoldable ["+", "*"])) [ BD.Ordered <$> (Gen.elements ")" $ L.singleton ".")]
      <*> (L.fromFoldable <$> tinyArrayOf (L.fromFoldable <$> (tinyArrayOf block0)))

inlines ∷ ∀ a. (SCA.Arbitrary a, BD.Value a) ⇒ Gen.Gen (Array (BD.Inline a))
inlines =
  Gen.oneOf inlines0
    [ A.singleton <$> link
    , A.singleton <$> formField
    ]
  where
  inlines0 ∷ Gen.Gen (Array (BD.Inline a))
  inlines0 =
    Gen.oneOf (A.singleton <$> simpleText)
     [ three
         <$> simpleText
         <*> (Gen.elements BD.Space $ L.fromFoldable [BD.SoftBreak, BD.LineBreak])
         <*> simpleText
     , A.singleton <$> (BD.Code <$> (Gen.elements true (L.singleton false)) <*> alphaNum)
     ]

  link ∷ Gen.Gen (BD.Inline a)
  link = BD.Link <$> (L.fromFoldable <$> inlines0) <*> linkTarget

  linkTarget ∷ Gen.Gen BD.LinkTarget
  linkTarget =
    Gen.oneOf (BD.InlineLink <$> alphaNum)
      [ BD.ReferenceLink <<< M.Just <$> alphaNum ]

  formField ∷ Gen.Gen (BD.Inline a)
  formField =
    BD.FormField
      <$> alphaNum
      <*> Gen.elements true (L.singleton false)
      <*> SCA.arbitrary

simpleText ∷ ∀ a. Gen.Gen (BD.Inline a)
simpleText = BD.Str <$> alphaNum

alphaNum ∷ Gen.Gen String
alphaNum = do
  len ← Gen.chooseInt 5 10
  S.fromCharArray <$> Gen.vectorOf len (Gen.elements (CH.fromCharCode 97) $ L.fromFoldable (S.toCharArray "qwertyuioplkjhgfdszxcvbnm123457890"))


main ∷ ∀ e. Eff (TestEffects e) Unit
main = do
  static
  generated
