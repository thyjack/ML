{-# LANGUAGE RecordWildCards #-}
module JML.Lang.Lexer where

import Text.Parsec.Token
import Text.Parsec.Language (haskellStyle)

reservedMLNames =
  [ "λ"
  , "ƒ", "fix"
  , "let", "in"
  ]

reservedMLOperators =
  [ "="
  ]

jmlLanguageDef = haskellStyle
  { reservedNames = reservedMLNames
  , reservedOpNames = reservedMLOperators
  }

lexer = makeTokenParser jmlLanguageDef

TokenParser {..} = lexer
