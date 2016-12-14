module Defs where

type Name = String

data Fix f a = In { unZip :: a, unFix :: f (Fix f a)}

instance Functor f => Functor (Fix f) where
  fmap g x = let z = unZip x
                 f = unFix x
              in In (g z) (fmap (fmap g) f)

run f x = let a = unZip x
              b = unFix x
           in f a (fmap (run f) b)

