module JML.Semantics.FixSpec (main, spec) where

import Test.Hspec

import JML.TestUtils

main = hspec spec

spec :: Spec
spec = 
  describe "Fix" $ do
    it "can type fix id (fix f. \\y. y)" $
      shouldBeRight (typeCheckOne "fix f. \\y. y")

    it "cannot type (fix f. \\y. f)" $
      shouldBeLeft (typeCheckOne "fix f. \\y. f")
