module Tests.Reservoir.Project where

import Clash.Explicit.Testbench (stimuliGenerator)
import Clash.Prelude
import Hedgehog as H
-- Import the module containing the @accum@ function
import Reservoir.Project
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import qualified Prelude as P


wInTest :: Vec 2 (Vec 1 Double)
wInTest =
  (3 :> Nil) :> 
  (3 :> Nil) :> 
  Nil -- every neuron gets input *1

wResTest :: Vec 2 (Vec 2 Double)
wResTest =
  (0 :> 0 :> Nil) :>
  (0 :> 0 :> Nil) :>
  Nil -- no recurrence for the first test

testOutput :: [Vec 2 Double]
testOutput = sampleN 4 $ mealy (reservoirT du dv bias vth wInTest wResTest) initS inputs
  where
    initS = repeat (NeuronState 0 6, 0)
    inputs = stimuliGenerator @4 systemClockGen resetGen ((0 :> Nil) :> repeat (0 :> Nil))

expected :: [Vec 2 Double]
expected =
  [ 1 :> 1 :> Nil, -- both spike on tick 0
    1 :> 1 :> Nil,
    0 :> 0 :> Nil,
    0 :> 0 :> Nil
  ]

prop_basic :: H.Property
prop_basic = H.property $ do
  P.take 4 testOutput H.=== expected

unitTests :: TestTree
unitTests = $(testGroupGenerator)
