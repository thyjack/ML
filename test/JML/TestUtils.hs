module JML.TestUtils where

import JML
import JML.Lang.Defs
import JML.Semantics.Defs

typeCheckOne :: String -> Either SErrors MLType
typeCheckOne = typeExpr . either undefined id . pparse

shouldBeLeft :: Either a b -> Bool
shouldBeLeft (Left _) = True
shouldBeLeft _        = False

shouldBeRight :: Either a b -> Bool
shouldBeRight = not . shouldBeLeft
