module JML.Semantics.AppSpec (main, spec) where

import Control.Monad (forM_)
import Test.Hspec

import JML.TestUtils

main = hspec spec

tests = 
  [ "(\\x. x) 3"
  , "(\\x y. y) 5 \"hi\""
  , "\\x y z. x z (y z)"
  ]

spec :: Spec
spec = 
  describe "App" $ do
    forM_ tests $ \t ->
      it ("can type " ++ t) $ 
        shouldBeRight (typeCheckOne t)
    
    it "cannot type self-application (\\x. x x)" $ 
      shouldBeLeft (typeCheckOne "\\x. x x")
