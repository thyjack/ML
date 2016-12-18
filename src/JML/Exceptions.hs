{-# LANGUAGE FlexibleContexts #-}
module JML.Exceptions where

class MLError e where
  formatError :: e -> String



