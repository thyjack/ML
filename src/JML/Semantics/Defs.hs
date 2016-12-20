{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
module JML.Semantics.Defs where

import Text.Parsec.Pos
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except

import JML.Lang.Defs
import JML.Utils

infixr 5 :->:
data MLType' a = Phi a 
               | Concrete String 
               | (MLType' a) :->: (MLType' a) 
               | ForAll [MLTVar] (MLType' a)
               deriving (Eq, Foldable)

type MLTVar = Int
type MLType = MLType' MLTVar

instance Show MLType where
  showsPrec _ (Phi n)      = showString ('t':show n)
  showsPrec _ (Concrete c) = showString c
  showsPrec i (a :->: b)
    | i > 0     = showString "(" . s . showString ")"
    | otherwise = s
    where s = showsPrec (i + 1) a . showString " -> " . shows b
  showsPrec i (ForAll tvs v)
    | i > 0     = showString "(" . s . showString ")"
    | otherwise = s
    where s = showString "∀ " . showString (unwords (map (\n -> 't':show n) tvs)) 
            . showString ". " . shows v 

type Counter   = [Int]
type Locations = [Expr SrcPos]
type SErrors   = [(SemanticError, Locations)]
data TypeState = 
  TypeState { counter :: Counter
            , locationStack :: Locations
            , errors :: SErrors
            }

class (MonadError SErrors m, MonadState TypeState m) => TypeMonad m where
  freshType :: m MLType
  returnType :: MLTVar -> m ()
  
  enterExpr :: Expr SrcPos -> m ()
  leaveExpr :: m ()

  reportError :: SemanticError -> m ()
  reportErrorF :: SemanticError -> m a

getLocs :: (TypeMonad m) => m Locations
getLocs = do TypeState { locationStack = locs } <- get
             return (take 3 locs)

errorLimit = 100

instance (MonadError SErrors m, MonadState TypeState m) => TypeMonad m where 
  freshType = 
    do ts@TypeState { counter = (n:ns) } <- get
       put $ ts { counter = ns }
       return (Phi n)
  returnType n = modify $ \ts@TypeState { counter = ns } -> ts { counter = n:ns }

  enterExpr expr =
    modify $ \ts@TypeState { locationStack = locs } -> ts { locationStack = expr:locs }
  leaveExpr =
    modify $ \ts@TypeState { locationStack = _:locs } -> ts { locationStack = locs }

  reportError e = 
    do locs <- getLocs
       modify $ \ts@TypeState { errors = errors } -> ts { errors = (e, locs):errors }
       TypeState { errors = errors } <- get
       when (length errors >= errorLimit) $ throwError errors
  reportErrorF e = 
    do reportError e
       TypeState { errors = errors } <- get
       throwError errors

type Subst = M.Map MLTVar MLType

-- Errors
--
data SemanticError =
    UnboundedTermError Name
  | UnificationError MLType MLType
  | OccursCheckError MLType MLType
  deriving Show

class Reportable a where
  report :: a -> String

instance Reportable SemanticError where
  report (UnboundedTermError name) = 
    "Unbounded term " ++ name ++ " found"
  report (UnificationError t1 t2) = 
    unlines [ "Cannot unify type:"
            , "  " ++ show t1
            , "and"
            , "  " ++ show t2
            ]
  report (OccursCheckError t1 t2) = 
    unlines [ "Occurs check failure: "
            , "  cannot deduce " ++ show t1 ++ " ~ " ++ show t2
            ]

formatLoc loc = pos ++ " the " ++ desp loc ++ ' ':show loc
  where
    pos = let sp = unZip loc
           in "(" ++ show (sourceLine sp) ++ ":" ++ show (sourceColumn sp) ++ ")"

    desp (unFix -> Term _)  = "term"
    desp (unFix -> Const _) = "const"
    desp (unFix -> Abs {})  = "abstraction"
    desp (unFix -> App {})  = "application"
    desp (unFix -> Let {})  = "let binding"
    desp (unFix -> Fix {})  = "fix pointer expression"

