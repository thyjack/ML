{-# LANGUAGE FlexibleContexts #-}
module Exceptions where

class MLError e where
  formatError :: e -> String

