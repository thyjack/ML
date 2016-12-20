module JML.Semantics.AbsSpec (main, spec) where

import Control.Monad (forM_)
import Test.Hspec

import JML.TestUtils

main = hspec spec

spec :: Spec
spec = 
  describe "Abs" $ do
    it "can type id" $
      shouldBeRight (typeCheckOne "\\x. x")

    it "can type const" $
      shouldBeRight (typeCheckOne "\\x y. x")
