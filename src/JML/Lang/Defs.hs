{-# LANGUAGE DeriveFunctor #-}
module JML.Lang.Defs where

import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except
import Text.ParserCombinators.Parsec (SourcePos)

import JML.Utils

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
 - Exceptions Workaround
 -}

data GenericMLError = GenericMLError String
instance Show GenericMLError where
  show (GenericMLError e) = e
