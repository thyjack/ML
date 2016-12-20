module JML.Semantics.LetSpec (main, spec) where

import Test.Hspec

import JML.TestUtils

main = hspec spec

spec :: Spec
spec = 
  describe "Let" $ do
    it "can type polymorphic self-application let y = \\x. x in y y" $
      shouldBeRight (typeCheckOne "let y = \\x. x in y y")

    it "cannot type self-application (\\x. let y = x in y y)" $
      shouldBeLeft (typeCheckOne "\\x. let y = x in y y")

    it "can type recursion let x = x in x" $
      shouldBeRight (typeCheckOne "let x = x in x")

    it "can type fix combinator (\\f. let x = f x in x)" $
      shouldBeRight (typeCheckOne "\\f. let x = f x in x")

    it "can type multi def let (let x = y, y = x in x)" $
      shouldBeRight (typeCheckOne "let x = y, y = x in x")
