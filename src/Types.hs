{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Types where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F
import Control.Monad.State (MonadState(..), modify)
import Control.Monad.Except

import Lang
import Defs
import Exceptions


infixr 5 <@>
(<@>) :: Subst -> Subst -> Subst
s1 <@> s2 = M.foldrWithKey merge s2 s1
  where 
    merge from to s2'
      | _ :->: _ <- to   = M.insert from to s2'
      | Concrete x <- to = M.insert from to s2'
    -- Phi x <- from and Phi y <- to
    merge from to s2' = 
      case M.lookup to s2' of
        Just to' -> (M.insert from to' . M.delete to) s2'
        Nothing  -> M.insert from to s2'
    
apply :: Subst -> MLType -> MLType
apply s t@(Phi _) = 
  fromMaybe t (M.lookup t s)
apply s t@(Concrete _) =
  t
apply s (t1 :->: t2) =
  apply s t1 :->: apply s t2

applyContext :: Subst -> Context -> Context
applyContext s = fmap (s `apply`)

occurs :: Int -> MLType -> Bool
occurs i (Concrete _) = False
occurs i (Phi n)      = i == n
occurs i (t1 :->: t2) = occurs i t1 || occurs i t2

unify :: Monad m => MLType -> MLType -> m Subst
unify (Phi x) (Phi y) = 
  return $ M.singleton (Phi x) (Phi y)
unify (Concrete c1) (Concrete c2)
  | c1 == c2 = return mempty
unify (Phi x) t
  | x `occurs` t = fail "occurs check failed"
  | otherwise    = return $ M.singleton (Phi x) t
unify t phi@(Phi _) = 
  unify phi t
unify (a :->: b) (c :->: d) =
  do s1 <- unify a c
     s2 <- unify (s1 `apply` b) (s1 `apply` d)
     return (s2 <@> s1)
unify t1 t2 = 
  fail $ concat ["cannot unify type ", show t1, " and ", show t2]

unifyContexts :: Monad m => Context -> Context -> m Subst
unifyContexts c1 c2
  | M.null c1 || M.null c2 = return mempty
  | otherwise
    = let intersect = M.intersectionWith (,) c1 c2
       in substitute (F.toList intersect)
  where
    substitute [] = return mempty
    substitute ((t1, t2): ts) =
      do s1 <- unify t1 t2
         s2 <- substitute (fmap (fork (s1 `apply`)) ts)
         return $ s2 <@> s1
    
    fork f (x, y) = (f x, f y)

typeLit :: Lit -> MLType
typeLit (LitInt _) = 
  Concrete "int"
typeLit (LitString _) = 
  Concrete "string"

{-
 -       
 - Const -------------
 -        Γ ⊢ c: v(c) 
 -
 -
 - Ax --------------
 -     Γ, x:τ ⊢ x:τ 
 -}

ppml :: (Monad m, TypeMonad m, MonadError e m) => Expr SrcPos -> m (Context, MLType)
ppml = run $ \_ exp ->
  case exp of
    Const lit -> return (mempty, typeLit lit)
    Term n ->
      do t <- freshType
         return (M.singleton n t, t)
    Abs n e ->
      do (ctx, t) <- e
         maybe (freshAbsType ctx t) (absType ctx t n) $ M.lookup n ctx
    App e1 e2 ->
      do t <- freshType
         (ctx1, t1) <- e1
         (ctx2, t2) <- e2
         s1 <- unify t1 (t2 :->: t)
         s2 <- unifyContexts (s1 `applyContext` ctx1) (s1 `applyContext` ctx2)
         let s = s2 <@> s1
             ctx = ctx1 `M.union` ctx2
         return (s `applyContext` ctx, s `apply` t)
  where
    freshAbsType ctx te =
      do t <- freshType
         return (ctx, t :->: te)
    absType ctx te n t =
      return (M.delete n ctx, t :->: te)

