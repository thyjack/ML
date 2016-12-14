module Utils where

indented :: Int -> String -> String
indented n = (replicate n ' ' ++)
