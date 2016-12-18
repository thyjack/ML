{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveFoldable #-}
module JML.Semantics.Defs where

import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except

import JML.Lang.Defs

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
data TypeState = 
  TypeState { counter :: Counter
            , locationStack :: Locations
            }

class (MonadError GenericMLError m, MonadState TypeState m) => TypeMonad m where
  freshType :: m MLType
  returnType :: MLTVar -> m ()
  
  enterExpr :: Expr SrcPos -> m ()
  leaveExpr :: m ()

getLocs :: (TypeMonad m) => m Locations
getLocs = do TypeState { locationStack = locs } <- get
             return (take 3 locs)

instance (MonadError GenericMLError m, MonadState TypeState m) => TypeMonad m where 
  freshType = 
    do ts@TypeState { counter = (n:ns) } <- get
       put $ ts { counter = ns }
       return (Phi n)
  returnType n = modify $ \ts@TypeState { counter = ns } -> ts { counter = n:ns }

  enterExpr expr =
    modify $ \ts@TypeState { locationStack = locs } -> ts { locationStack = expr:locs }
  leaveExpr =
    modify $ \ts@TypeState { locationStack = _:locs } -> ts { locationStack = locs }

type Subst   = M.Map MLTVar MLType
