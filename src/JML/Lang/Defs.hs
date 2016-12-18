{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module JML.Lang.Defs where

import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except
import Text.ParserCombinators.Parsec (SourcePos)

import JML.Utils

type Name = String

{-
 - Expressions
 -}

data Lit  = LitInt Integer
          | LitDouble Double
          | LitString String

instance Show Lit where
  show (LitInt i)    = 'i':show i
  show (LitDouble d) = 'd':show d
  show (LitString s) = 's':show s

data MLProg a = MLProg [(Name, Expr a)]
data ML e = Term Name
          | Const Lit
          | Abs [Name] e
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
data MLType' a = Phi a 
               | Concrete String 
               | (MLType' a) :->: (MLType' a) 
               | ForAll [MLTVar] (MLType' a)
               deriving (Eq, Foldable)

type MLTVar = Int
type MLType = MLType' MLTVar


{- 
instance Ord MLType where
  compare (Phi x) (Phi y) = compare x y
  compare _ _             = error "only phi types can be compared"
-}

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

{-
 - Exceptions Workaround
 -}

data GenericMLError = GenericMLError String
instance Show GenericMLError where
  show (GenericMLError e) = e
