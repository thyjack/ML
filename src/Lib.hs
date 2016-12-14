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

Right test1 = parse parseML "<none>" "hello"
Right test2 = parse parseML "<none>" "(λx. x)"
Right test3 = parse parseML "<none>" "(λx. (λy. y))"
Right test4 = parse parseML "<none>" "(λx. x) 3"
Right test5 = parse parseML "<none>" "(λx. (λy. y)) 5 \"hi\""
Right test6 = parse parseML "<none>" "λx.(λy.x x y)"

runString = run $ \_ a _ ->
  case a of 
    Term n    -> n
    Const l   -> show l
    Abs n e   -> concat ["ƛ", n, ". ", e]
    App e1 e2 -> concat ["(", e1, ")", "(", e2, ")"]

instance Show (Expr SrcPos) where
  show = runString

getType :: Expr SrcPos -> Either GenericMLError (Context, MLType)
getType e = evalStateT (ppml e) (TypeState 0 [])

simpleTest =
  putStrLn "" >>
  forM_ [test1, test2, test3, test4, test5, test6] (\test -> 
      do putStrLn $ "test for expression: " ++ show test
         let result = getType test
         case result of
           Right (c, t) -> do
             putStrLn $ "context: " ++ show c
             putStrLn "(context should be empty for closed terms)"
             putStrLn $ "type: " ++ show t
             putStrLn ""
           Left e -> putStrLn $ (unlines . map (indented 2) . lines . show) e
    ) >>
  putStrLn ""
