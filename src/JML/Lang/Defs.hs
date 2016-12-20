{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module JML.Lang.Defs where

import Data.List (intercalate)
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
          | Let [(Name, e)] e
          | Fix Name e
          deriving Functor
type Expr = Fix ML
type ExprUnfold = ML (Expr SrcPos)

instance Show e => Show (ML e) where
  showsPrec _ (Term n)   = showString n
  showsPrec _ (Const l)   = shows l
  showsPrec i (Abs ns e) = showParened i s
    where 
      s = showString "λ" 
        . showString (unwords ns) 
        . showString ". " 
        . shows e
  showsPrec i (App e1 e2) =
    showsPrec (succ i) e1 . showString " " . showsPrec (succ i) e2
  showsPrec i (Let nes e) = showParened i s
    where
      s = showString "let "
        . showString (intercalate ", " $ map (\(n, e) -> n ++ " = " ++ show e) nes)
        . showString " in "
        . shows e
  showsPrec i (Fix n e) = showParened i s
    where
      s = showString "fix " . showString n . showString ". " . shows e

instance Show (Expr a) where
  showsPrec i = showsPrec i . unFix

type SrcPos = SourcePos

{-
 - Exceptions Workaround
 -}

data GenericMLError = GenericMLError String
instance Show GenericMLError where
  show (GenericMLError e) = e
