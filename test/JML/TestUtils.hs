{-# LANGUAGE RecordWildCards #-}
module JML.TestUtils where

import Text.Parsec (parse, ParseError)
import Control.Monad.State (runStateT)

import JML.Lang.Defs
import JML.Lang.Parser (parseML, parseProg)
import JML.Semantics.Defs
import JML.Semantics.Types (milner)

pparse :: String -> Either ParseError (Expr SrcPos)
pparse = parse parseML "<none>"

getType :: Expr SrcPos -> Either SErrors MLType
getType e = 
  case runStateT (milner e) (TypeState [0..] [] []) of
    Left es -> Left es
    Right (a, s) ->
      let TypeState {..} = s
       in if null errors then Right a else Left errors

typeCheckOne :: String -> Either SErrors MLType
typeCheckOne = getType . either undefined id . pparse

shouldBeLeft :: Either a b -> Bool
shouldBeLeft (Left _) = True
shouldBeLeft _        = False

shouldBeRight :: Either a b -> Bool
shouldBeRight = not . shouldBeLeft
