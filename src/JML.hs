{-# LANGUAGE RecordWildCards #-}
module JML where

import Text.Parsec
import Control.Monad.State

import JML.Lang.Defs
import JML.Lang.Parser
import JML.Semantics.Defs
import JML.Semantics.Types
import JML.Utils

pparse :: String -> Either ParseError (Expr SrcPos)
pparse = parse parseML "<none>"

typeExpr :: Expr SrcPos -> Either SErrors MLType
typeExpr e = 
  case runStateT (milner e) (TypeState [0..] [] []) of
    Left es -> Left es
    Right (a, s) ->
      let TypeState {..} = s
       in if null errors then Right a else Left errors

parseProg :: String -> Either ParseError (Expr SrcPos)
parseProg = fmap makeProg . parse parseMLProg "<none>"
  where
    makeLet prog = In undefined (Let prog (In undefined (Term "main")))
    makeProg (MLProg prog) = makeLet prog

typeProg :: MLProg SrcPos -> Either SErrors MLType
typeProg s = undefined

checkTypeForExpr :: String -> IO ()
checkTypeForExpr s =
  let (Right expr) = pparse s
      typeOrError = typeExpr expr
   in putStrLn ("Type for " ++ s ++ ": ") >>
      case typeOrError of
        Left errors -> showErrors errors
        Right t     -> print t
  where
    showErrors es = forM_ es $ \err -> 
      putStrLn $ formatError err
         
