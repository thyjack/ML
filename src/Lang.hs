{-# LANGUAGE RecordWildCards #-}
module Lang (
  parseML
) where

import Debug.Trace
import Text.ParserCombinators.Parsec
import Control.Monad ((=<<), forM_)

import Defs

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

reserved =
  [ "λ", "\\"
  , "ƒ", "fix"
  , "let", "in"
  ]

rejectReserved = forM_ reserved (notFollowedBy . string)

parseName :: Parser Name
parseName = try $ 
  do rejectReserved
     c <- letter
     cs <- many (letter <|> digit <|> char '_')
     let name = c:cs
     return name

parseTerm :: Parser ExprUnfold
parseTerm = Term <$> parseName

parseExprAfterDot = char '.' >> spaces >> parseExpr

parseAbs :: Parser ExprUnfold
parseAbs = 
  do char 'λ' <|> char '\\'
     n <- parseName
     ns <- many (spaces >> parseName)
     exp <- parseExprAfterDot
     return (Abs (n:ns) exp)

parseFix :: Parser ExprUnfold
parseFix =
  do string "ƒ" <|> try (string "fix")
     spaces
     n <- parseName
     exp <- parseExprAfterDot
     return (Fix n exp)

parseLet :: Parser ExprUnfold
parseLet =
  do string "let"
     n <- spaces >> parseName
     spaces >> char '='
     e1 <- spaces >> parseExpr
     spaces >> string "in"
     e2 <- spaces >> parseExpr
     return (Let n e1 e2)

parseConst :: Parser ExprUnfold
parseConst = parseInt <|> parseString
  where
    parseInt    = Const . LitInt . read <$> many1 digit
    parseString = 
      do char '"'
         s <- many (noneOf "\"")
         char '"'
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
parseParen = 
  do char '('
     e <- parseExpr
     char ')'
     return e

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

