module JML.Semantics.AxSpec (main, spec) where

import Test.Hspec

import JML.TestUtils

main = hspec spec

spec :: Spec
spec =
  describe "Ax" $ 
    it "should raise error when a term is unbounded" $ 
      shouldBeLeft (typeCheckOne "hello")
       
