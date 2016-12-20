#!/usr/bin/env ruby

if __FILE__ == $0
  package, test = ARGV
  package = package[0].capitalize + package[1..-1]
  test = test[0].capitalize + test[1..-1]

  data = DATA.read % {package: package, test: test}
  File::write("test/JML/#{package}/#{test}.hs", data)
end

__END__
module JML.%{package}.%{test} (main, spec) where

import Test.Hspec

import JML.TestUtils

main = hspec spec

spec :: Spec
spec = undefined
