{-# LANGUAGE RecordWildCards #-}
module JML.Lang.Parser (
  parseML
, parseProg
) where

import Debug.Trace
import Text.ParserCombinators.Parsec 
  ( Parser 
  , try
  , getPosition
  , pzero
  , char
  , (<|>)
  , choice
  , many
  , many1
  , eof
  , noneOf
  , sepEndBy
  , spaces
  )
import Control.Monad ((=<<), forM_, void)

import JML.Lang.Defs
import JML.Utils
import JML.Lang.Lexer

data ParseOptions = ParseOptions 
  { tryTerm :: Bool
  , tryAbs :: Bool
  , tryConst :: Bool
  , tryApp :: Bool
  , tryLet :: Bool
  , tryFix :: Bool
  }
defaultParseOptions =
  ParseOptions True True True True True True
onOption t m = 
  if t then m else pzero

touch p = 
  do pos <- getPosition
     ml <- p
     return (In pos ml)

infixr 5 <!>
a <!> b = void a <|> void b

parseTerm :: Parser ExprUnfold
parseTerm = Term <$> identifier

parseAbs :: Parser ExprUnfold
parseAbs = 
  do reserved "λ" <!> char '\\'
     n <- identifier
     ns <- many identifier
     dot
     exp <- parseExpr
     return (Abs (n:ns) exp)

parseFix :: Parser ExprUnfold
parseFix =
  do reserved "ƒ" <|> reserved "fix"
     n <- identifier
     dot
     exp <- parseExpr
     return (Fix n exp)

parseLet :: Parser ExprUnfold
parseLet =
  do reserved "let"
     n <- identifier
     reservedOp "="
     e1 <- parseExpr
     reserved "in"
     e2 <- parseExpr
     return (Let n e1 e2)

parseConst :: Parser ExprUnfold
parseConst = parseInt <|> parseString
  where
    parseInt    = Const . either LitInt LitDouble <$> naturalOrFloat
    parseString = 
      do s <- stringLiteral
         return (Const (LitString s))

parseApp :: Parser (Expr SrcPos)
parseApp =  try $
  do e1 <- parseOperand
     es <- many1 (spaces >> parseOperand)
     return (buildApp (e1:es))
  where 
    parseOperand = parseExpr1 (defaultParseOptions { tryApp = False })

    buildApp = foldl1 (\app e -> let p = unZip app in In p (App app e))

parseParen :: Parser (Expr SrcPos)
parseParen = parens parseExpr

parseExpr = parseExpr1 defaultParseOptions

parseExpr1 :: ParseOptions -> Parser (Expr SrcPos)
parseExpr1 ParseOptions{..} = 
  onOption tryApp parseApp <|>
  parseParen <|>
  touch ( choice
          [ 
            onOption tryAbs parseAbs
          , onOption tryLet parseLet
          , onOption tryFix parseFix
          , onOption tryTerm parseTerm
          , onOption tryConst parseConst
          ]
        ) 
  
parseML = parseExpr <* eof

parseDef :: Parser (Name, Expr SrcPos)
parseDef =
  do whiteSpace
     name <- identifier
     reservedOp "="
     exp <- parseExpr
     return (name, exp)

parseProg :: Parser (MLProg SrcPos)
parseProg = MLProg <$> sepEndBy parseDef (char ';') <* eof


