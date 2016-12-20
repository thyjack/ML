{-# LANGUAGE RecordWildCards #-}
module JML.Semantics.Context where

import qualified Data.Map as M
import qualified Data.IntSet as S

import JML.Semantics.Defs
import JML.Utils

data Context = MLContext
  { ctxMap :: M.Map Name MLType
  , ctxOccurences :: S.IntSet 
  } deriving Show

freeVars :: MLType -> S.IntSet
freeVars = go
  where
    go (Phi x)       = S.singleton x
    go (Concrete _)  = mempty
    go (a :->: b)    = go a `S.union` go b
    go (ForAll ns a) = foldr S.delete (go a) ns

rebuildContext :: M.Map Name MLType -> Context
rebuildContext m = 
  let occurences = foldr (\a -> (freeVars a `S.union`)) S.empty m
   in MLContext m occurences

empty :: Context
empty = MLContext mempty mempty

insert :: Name -> MLType -> Context -> Context
insert k v MLContext {..} =
  let newMap = M.insert k v ctxMap
      fv = freeVars v
      newOccurences = ctxOccurences `S.union` fv
   in MLContext newMap newOccurences

lookup :: Name -> Context -> Maybe MLType
lookup n = M.lookup n . ctxMap

isFree :: MLTVar -> Context -> Bool
isFree t MLContext {..} = t `S.notMember` ctxOccurences

update :: (MLType -> MLType) -> Name -> Context -> Context
update f n = rebuildContext . M.update (Just . f) n . ctxMap

without :: Name -> Context -> Context
without n = rebuildContext . M.delete n . ctxMap

