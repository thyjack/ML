module Exceptions where

import Defs

class MLError e where
  formatError :: e -> String

data UnificationError = UnificationError 
