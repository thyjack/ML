{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Lib where

import Control.Monad.State
import Text.ParserCombinators.Parsec

import JML.Lang.Parser
import JML.Types
import JML.Lang.Defs
import JML.Exceptions
import JML.Utils

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
    Abs ns e  -> concat ["ƛ", unwords ns, ". ", e]
    App e1 e2 -> concat ["(", e1, ")", "(", e2, ")"]
    Fix g e   -> concat ["fix ", g, " . ", e]

instance Show (Expr SrcPos) where
  show = runString

getType :: Expr SrcPos -> Either GenericMLError MLType
getType e = evalStateT (milner e) (TypeState [0..] [])

runOneTest test = 
  do putStrLn $ "test for expression: " ++ show test
     let result = either (error . show) getType $ parse parseML "<none>" test
     case result of
       Right t -> do
         putStrLn $ "type: " ++ show t
         putStrLn ""
       Left e -> putStrLn $ (unlines . map (indented 2) . lines . show) e

simpleTest =
  putStrLn "" >>
  forM_ [test1, test2, test3, test4, test5, test6] runOneTest >>
  putStrLn ""
