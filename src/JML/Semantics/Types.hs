{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module JML.Semantics.Types where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F
import Control.Monad.State (MonadState(..), modify)
import Control.Monad.Except

import Debug.Trace

import JML.Semantics.Context (Context (..))
import qualified JML.Semantics.Context as C
import JML.Semantics.Defs
import JML.Lang.Parser
import JML.Lang.Defs
import JML.Exceptions
import JML.Utils

throwSemanticError = undefined

infixr 5 <@>
(<@>) :: Subst -> Subst -> Subst
s1 <@> s2 = M.foldrWithKey merge (fmap (s1 `apply`) s2) s1
  where 
    merge from to s2'
      | _ :->: _ <- to   = M.insert from to s2'
      | Concrete x <- to = M.insert from to s2'
    -- Phi x <- from and Phi y <- to
    merge from to s2' 
      | Phi to' <- to =
        case M.lookup to' s2' of
          Just to'' -> (M.insert from to'' . M.delete to') s2'
          Nothing   -> M.insert from to s2'
    
apply :: Subst -> MLType -> MLType
apply s t@(Phi v) = 
  fromMaybe t (M.lookup v s)
apply s t@(Concrete _) =
  t
apply s (ForAll ns t) =
  ForAll ns (apply s t)
apply s (t1 :->: t2) =
  apply s t1 :->: apply s t2

applyContext :: Subst -> Context -> Context
applyContext s MLContext {..} = 
  let newMap = fmap (s `apply`) ctxMap
   in C.rebuildContext newMap

occurs :: Int -> MLType -> Bool
occurs i (Concrete _) = False
occurs i (Phi n)      = i == n
occurs i (t1 :->: t2) = occurs i t1 || occurs i t2

unify :: TypeMonad m => MLType -> MLType -> m Subst
unify (Phi x) (Phi y) = 
  return $ M.singleton x (Phi y)
unify (Concrete c1) (Concrete c2)
  | c1 == c2 = return mempty
unify (Phi x) t
  | x `occurs` t = reportError (OccursCheckError (Phi x) t) >> return mempty
  | otherwise    = return $ M.singleton x t
unify t phi@(Phi _) = 
  unify phi t
unify (a :->: b) (c :->: d) =
  do s1 <- unify a c
     s2 <- unify (s1 `apply` b) (s1 `apply` d)
     return (s2 <@> s1)
unify t1 t2 = reportError (UnificationError t1 t2) >> return mempty

unifyContexts :: TypeMonad m => Context -> Context -> m Subst
unifyContexts (ctxMap -> c1) (ctxMap -> c2)
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

milner :: TypeMonad m => Expr SrcPos -> m MLType
milner e = snd <$> milner' e C.empty


-- | a slightly different version of milner's algorithm
--
-- relevent derivations include:
--
-- Ax ----------------
--     Γ, x: A ⊢ x: A 
--
-- C --------------
--    Γ ⊢ c: v(c)
--
--        Γ, x: A ⊢ E: B
-- ->I --------------------
--      Γ ⊢ \\x. E: A -> B
--
--      Γ ⊢ E1: A -> B  Γ ⊢ E2: A 
-- ->E ---------------------------
--           Γ ⊢ E1 E2: B
--
--      Γ ⊢ E1: A  Γ, x: ∀φ. A ⊢ E2: B
-- let --------------------------------  (φ not in Γ)
--         Γ ⊢ let x = E1 in E2: B
--
--
--          Γ, x: φ ⊢ E1: A  Γ, x: ∀φ. A ⊢ E2: B
-- let-rec --------------------------------------  (φ not in Γ)
--                Γ ⊢ let x = E1 in E2: B
--
--      Γ, g: A ⊢ E: A
-- fix -----------------
--      Γ ⊢ fix g. E: A
--
--       Γ ⊢ E: t
-- ∀I -------------- (φ not (free) in Γ)
--     Γ ⊢ E: ∀φ. t
--
--     Γ ⊢ E: ∀φ. t
-- ∀E --------------- (φ not (free) in Γ)
--     Γ ⊢ E: t[A/φ]
--

milner' :: TypeMonad m => Expr SrcPos -> (Context -> m (Subst, MLType))
milner' = run $ \_ exp fixExp ctx ->
  scoped fixExp $ 
    case exp of
      Const lit -> return (mempty, typeLit lit)
      Term n ->
        case C.lookup n ctx of 
          Just t  -> instantiate t >>= \t' -> return (mempty, t')
          Nothing -> reportErrorF $ UnboundedTermError n
      Abs ns e ->
        do ts <- mapM (const freshType) ns
           (s1, a) <- e (foldr (uncurry C.insert) ctx (zip ns ts))
           return (s1, s1 `apply` foldr (:->:) a ts)
      App e1 e2 ->
        do t <- freshType
           (s1, a) <- e1 ctx
           (s2, b) <- e2 (s1 `applyContext` ctx)
           s3 <- unify (s2 `apply` a) (b :->: t)
           let t' = s3 `apply` t
           recycle t t'
           return (s3 <@> s2 <@> s1, t')
      Let nes e -> 
        do ts <- mapM (const freshType) nes
           let ctx' = foldr (uncurry C.insert) ctx (zip (map fst nes) ts)
           (s1, ctx'') <- checkLet nes ctx'
           (s2, t) <- e ctx''
           return (s2 <@> s1, t)
      Fix g e ->
        do t <- freshType
           (s1, a) <- e (C.insert g t ctx)
           let t' = s1 `apply` t
           s2 <- unify t' a
           recycle t t'
           return (s2 <@> s1, s2 `apply` a)
  where
    checkLet [] ctx = return (mempty, ctx)
    checkLet ((n,e):nes) ctx = 
      do (s1, t) <- e ctx
         let ctx' = s1 `applyContext` ctx
         s2 <- unify t (fromMaybe undefined $ n `C.lookup` ctx')
         let ctx'' = s2 `applyContext` ctx'
         let t' = fromMaybe undefined $ n `C.lookup` ctx''
         let t'' = quantify t' (n `C.without` ctx'')
         let s = s2 <@> s1
         
         (s', ctx''') <- checkLet nes (C.update (const t'') n ctx'')
         return (s' <@> s, ctx''')

    instantiate t
      | ForAll ts t' <- t = 
        do ts' <- mapM (const freshType) ts
           let s = foldr (uncurry M.insert) mempty (zip ts ts')
           return (s `apply` t')
      | otherwise = return t
 
    quantify t ctx =
      let ts = foldr (\tv ts' -> 
                 if tv `C.isFree` ctx then tv:ts' else ts'
               ) [] t
       in if null ts then t else ForAll ts t

    recycle t ts'
      | Phi n <- t, not (n `occurs` ts') = returnType n
      | otherwise = return ()

    scoped expr me =
      do enterExpr expr
         r <- me
         leaveExpr
         return r

