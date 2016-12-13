{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib where

import Control.Monad.State
import Text.ParserCombinators.Parsec

import Utils
import Lang
import Types
import Defs

{-
 - Debugging stuff
 -}

Right test1 = parse parseML "<none>" "hello"
Right test2 = parse parseML "<none>" "(λx. x)"
Right test3 = parse parseML "<none>" "(λx. (λy. y))"
Right test4 = parse parseML "<none>" "(λx. x) 3"
Right test5 = parse parseML "<none>" "(λx. (λy. y)) 5 \"hi\""
Right test6 = parse parseML "<none>" "λx.(λy.x x y)"

runString = run $ \_ a ->
  case a of 
    Term n    -> n
    Const l   -> show l
    Abs n e   -> concat ["ƛ", n, ". ", e]
    App e1 e2 -> concat ["(", e1, ")", "(", e2, ")"]

instance Show (Expr SrcPos) where
  show = runString

simpleTest =
  putStrLn "" >>
  forM_ [test1, test2, test3, test4, test5, test6] (\t -> 
      do putStrLn $ "test for expression: " ++ show t
         (c, t) <- evalStateT (ppml t) (TypeState 0)
         putStrLn $ "context: " ++ show c
         putStrLn "(context should be empty for closed terms)"
         putStrLn $ "type: " ++ show t
         putStrLn ""
    ) >>
  putStrLn ""
