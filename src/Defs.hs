{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Defs where

import qualified Data.Map as M
import Control.Monad.State
import Text.ParserCombinators.Parsec (SourcePos)

type Name = String

data Fix f a = In { unZip :: a, unFix :: f (Fix f a)}

instance Functor f => Functor (Fix f) where
  fmap g x = let z = unZip x
                 f = unFix x
              in In (g z) (fmap (fmap g) f)

run f x = let a = unZip x
              b = unFix x
           in f a (fmap (run f) b)

{-
 - Expressions
 -}

data Lit  = LitInt Int
          | LitString String

instance Show Lit where
  show (LitInt i)    = 'i':show i
  show (LitString s) = 's':show s

data ML e = Term Name
          | Const Lit
          | Abs Name e
          | App e e
          | Let Name e e
          | Fix Name e
          deriving Functor
type Expr = Fix ML
type ExprUnfold = ML (Expr SrcPos)

type SrcPos = SourcePos

{-
 - Types
 -}

infixr 5 :->:
data MLType = Phi Int | Concrete String | MLType :->: MLType
            deriving Eq

instance Ord MLType where
  compare (Phi x) (Phi y) = compare x y
  compare _ _             = error "only phi types can be compared"

instance Show MLType where
  showsPrec _ (Phi n)      = showString ('t':show n)
  showsPrec _ (Concrete c) = showString c
  showsPrec i (a :->: b)
    | i > 0     = showString "(" . s . showString ")"
    | otherwise = s
    where s = showsPrec (i + 1) a . showString " -> " . shows b

type Counter   = Int
type Location  = [Expr SrcPos]
data TypeState = 
  TypeState { counter :: Counter
            , locationStack :: Location
            }

class MonadState TypeState m => TypeMonad m where
  freshType :: m MLType
  
  enterExpr :: Expr SrcPos -> m ()
  leaveExpr :: m ()

instance MonadState TypeState m => TypeMonad m where 
  freshType = 
    do ts@TypeState { counter = counter } <- get
       put $ ts { counter = succ counter }
       return (Phi counter)

  enterExpr expr =
    modify $ \ts@TypeState { locationStack = locs } -> ts { locationStack = expr:locs }
  leaveExpr =
    modify $ \ts@TypeState { locationStack = _:locs } -> ts { locationStack = locs }

type Subst   = M.Map MLType MLType
type Context = M.Map Name MLType


