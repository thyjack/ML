{-# LANGUAGE RecordWildCards #-}
module Lang (
  parseML
) where

import Debug.Trace
import Text.ParserCombinators.Parsec
import Control.Monad ((=<<))

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

parseName :: Parser Name
parseName = 
  do c <- letter
     cs <- many (letter <|> digit <|> char '_')
     return (c:cs)

parseTerm :: Parser ExprUnfold
parseTerm = Term <$> parseName

parseAbs :: Parser ExprUnfold
parseAbs = 
  do char 'λ'
     n <- parseName
     char '.'
     spaces
     exp <- parseExpr
     return (Abs n exp)

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
          , onOption tryTerm parseTerm
          , onOption tryConst parseConst
          ]
        ) 
  
parseML = parseExpr <* eof

