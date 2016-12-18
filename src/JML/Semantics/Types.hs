{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module JML.Semantics.Types where

import Text.Parsec.Pos
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

throwMLError :: (MLError e, TypeMonad m) => (Locations -> e) -> m a
throwMLError fe = getLocs >>= \locs -> throwError (GenericMLError (formatError (fe locs)))

data UnboundedTermError = UnboundedTermError Name Locations
instance MLError UnboundedTermError where
  formatError (UnboundedTermError name locs) = 
    unlines $ [ "Unbounded term " ++ name ++ " found"
              , "In "
              ] ++ map (indented 2. formatLoc) locs

data UnificationError = UnificationError MLType MLType Locations
instance MLError UnificationError where
  formatError (UnificationError t1 t2 locs) = 
    unlines $ [ "Cannot unify type:"
              , "  " ++ show t1
              , "and"
              , "  " ++ show t2
              , "In "
              ] ++ map (indented 2. formatLoc) locs

data OccursCheckError = OccursCheckError MLType MLType Locations
instance MLError OccursCheckError where
  formatError (OccursCheckError t1 t2 locs) = 
    unlines $ [ "Occurs check failure: "
              , "  cannot deduce " ++ show t1 ++ " ~ " ++ show t2
              , "In "
              ] ++ map (indented 2 . formatLoc) locs

formatLoc loc = pos ++ " the " ++ desp loc ++ ' ':exprString
  where
    pos = let sp = unZip loc
           in "(" ++ show (sourceLine sp) ++ ":" ++ show (sourceColumn sp) ++ ")"

    exprString = flip run loc $ \_ expr _ ->
      case expr of
        Term n      ->  n
        Const l     ->  show l
        Abs ns e    ->  concat ["ƛ", unwords ns, ". ", e]
        App e1 e2   ->  concat ["(", e1, ")", "(", e2, ")"]
        Let n e1 e2 ->  concat ["let ", n, " = ", e1, " in ", e2]
        Fix g e     ->  concat ["fix ", g, " . ", e]

    desp (unFix -> Term _)  = "term"
    desp (unFix -> Const _) = "const"
    desp (unFix -> Abs {})  = "abstraction"
    desp (unFix -> App {})  = "application"
    desp (unFix -> Let {})  = "let binding"
    desp (unFix -> Fix {})  = "fix pointer expression"

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
  | x `occurs` t = throwMLError $ OccursCheckError (Phi x) t
  | otherwise    = return $ M.singleton x t
unify t phi@(Phi _) = 
  unify phi t
unify (a :->: b) (c :->: d) =
  do s1 <- unify a c
     s2 <- unify (s1 `apply` b) (s1 `apply` d)
     return (s2 <@> s1)
unify t1 t2 = throwMLError $ UnificationError t1 t2

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

milner' :: TypeMonad m => Expr SrcPos -> (Context -> m (Subst, MLType))
milner' = run $ \_ exp fixExp ctx ->
  scoped fixExp $ 
    case exp of
      Const lit -> return (mempty, typeLit lit)
      Term n ->
        case C.lookup n ctx of 
          Just t  -> instantiate t >>= \t' -> return (mempty, t')
          Nothing -> throwMLError $ UnboundedTermError n
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
      Let n e1 e2 ->
        do t <- freshType
           (s1, a) <- e1 (C.insert n t ctx)
           let t' = s1 `apply` t
           s2 <- unify t' a
           recycle t (s2 `apply` a)
           let s' = s2 <@> s1
           let ctx' = s' `applyContext` ctx
           let quantified = quantify a ctx'
           (s3, b) <- e2 (C.insert n quantified ctx')
           return (s3 <@> s', s3 `apply` b)
      Fix g e ->
        do t <- freshType
           (s1, a) <- e (C.insert g t ctx)
           let t' = s1 `apply` t
           s2 <- unify t' a
           recycle t t'
           return (s2 <@> s1, s2 `apply` a)
  where
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

