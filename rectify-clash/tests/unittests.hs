import Prelude

import Test.Tasty

import qualified Tests.Reservoir.Project

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Reservoir.Project.unitTests
  ]
