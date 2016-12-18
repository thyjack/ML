{-# LANGUAGE RecordWildCards #-}
module JML.Semantics.Context where

import qualified Data.Map as M
import qualified Data.IntSet as S

import JML.Lang.Defs

data Context = MLContext
  { ctxMap :: M.Map Name MLType
  , ctxOccurences :: M.Map MLTVar Int
  } deriving Show

freeVars :: MLType -> [MLTVar]
freeVars = S.toList . go
  where
    go (Phi x)       = S.singleton x
    go (Concrete _)  = mempty
    go (a :->: b)    = go a `S.union` go b
    go (ForAll ns a) = foldr S.delete (go a) ns

empty :: Context
empty = MLContext mempty mempty

insert :: Name -> MLType -> Context -> Context
insert k v MLContext {..} =
  let newMap = M.insert k v ctxMap
      fv = freeVars v
      newOccurences = 
        foldr ((flip .) M.insertWith (+) 1) ctxOccurences fv
   in MLContext newMap newOccurences

lookup :: Name -> Context -> Maybe MLType
lookup n = M.lookup n . ctxMap

isFree :: MLTVar -> Context -> Bool
isFree t MLContext {..} = t `M.notMember` ctxOccurences
