{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Lib where

import Control.Monad.State
import Text.ParserCombinators.Parsec

import Lang
import Types
import Defs
import Exceptions
import Utils

{-
 - Debugging stuff
 -}

test1 = "hello"
test2 = "(λx. x)"
test3 = "(λx. (λy. y))"
test4 = "(λx. x) 3"
test5 = "(λx. (λy. y)) 5 \"hi\""
test6 = "λx. λy. λz. x z (y z)"

runString = run $ \_ a _ ->
  case a of 
    Term n    -> n
    Const l   -> show l
    Abs n e   -> concat ["ƛ", n, ". ", e]
    App e1 e2 -> concat ["(", e1, ")", "(", e2, ")"]

instance Show (Expr SrcPos) where
  show = runString

getType :: Expr SrcPos -> Either GenericMLError (Subst, MLType)
getType e = evalStateT (milner e) (TypeState 0 [])

runOneTest test = 
  do putStrLn $ "test for expression: " ++ show test
     let result = either undefined getType $ parse parseML "<none>" test
     case result of
       Right (c, t) -> do
         putStrLn $ "substitution: " ++ show c
         putStrLn $ "type: " ++ show t
         putStrLn ""
       Left e -> putStrLn $ (unlines . map (indented 2) . lines . show) e

simpleTest =
  putStrLn "" >>
  forM_ [test1, test2, test3, test4, test5, test6] runOneTest >>
  putStrLn ""
