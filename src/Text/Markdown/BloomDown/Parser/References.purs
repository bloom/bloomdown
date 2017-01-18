module Text.Markdown.BloomDown.Parser.References
  ( parseLinkReference
  ) where

import Prelude

import Data.Array as A
import Data.Either as E
import Data.Maybe as M
import Data.String as S

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

import Text.Markdown.BloomDown.Parser.Utils as PU
import Text.Markdown.BloomDown.Syntax as BD

parseLinkReference ∷ ∀ a. String → M.Maybe (BD.Block a)
parseLinkReference = E.either (const M.Nothing) M.Just <<< flip P.runParser linkReference

linkReference ∷ ∀ a. P.Parser String (BD.Block a)
linkReference = do
  l ←
    charsToString <$> do
      PS.string "["
      PU.skipSpaces
      PC.manyTill PS.anyChar (PS.string "]")
  PS.string ":"
  PU.skipSpaces
  uri ← charsToString <$> PC.manyTill PS.anyChar PS.eof
  pure $ BD.LinkReference l uri

  where
    charsToString =
      S.trim
        <<< S.fromCharArray
        <<< A.fromFoldable
