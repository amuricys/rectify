module Tests.Reservoir.Project where

import Clash.Explicit.Testbench (stimuliGenerator)
import Clash.Prelude
import Hedgehog as H
-- Import the module containing the @accum@ function
import Reservoir.Project (NeuronState (..), reservoirT)
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import qualified Prelude as P

-- -- Define a Hedgehog property to test the @accum@ function
-- prop_accum :: H.Property
-- prop_accum = H.property $ do

--   -- Simulate for a random duration between 1 and 100 cycles
--   simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

--   -- Generate a list of random unsigned numbers.
--   inp <- H.forAll
--     (Gen.list (Range.singleton simDuration)
--     (genUnsigned Range.linearBounded))
--   let

--     -- Simulate the @accum@ function for the pre-existing @System@ domain
--     -- and 8 bit unsigned numbers.
--     --
--     -- The (hidden) reset input of @accum@ will be asserted in the first cycle;
--     -- during this cycle it will emit its initial value and the input is
--     -- ignored. So we need to present a dummy input value.
--     simOut = C.sampleN (simDuration + 1) (accum @C.System @8 (C.fromList (0:inp)))
--     -- Calculate the expected output. The first cycle is the initial value, and
--     -- the result of the final input value does not appear because the
--     -- accumulator has 1 cycle latency.
--     expected = 0 : init (scanl (+) 0 inp)

--   -- Check that the simulated output matches the expected output
--   simOut H.=== expected

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
testOutput = sampleN 40 $ mealy (reservoirT wInTest wResTest) initS inputs
  where
    initS = repeat (NeuronState 0 6, 0)
    inputs = stimuliGenerator @40 systemClockGen resetGen ((0 :> Nil) :> repeat (0 :> Nil))

expected :: [Vec 2 Double]
expected =
  [ 1 :> 1 :> Nil, -- both spike on tick 0
    0 :> 0 :> Nil,
    0 :> 0 :> Nil,
    0 :> 0 :> Nil
  ]

prop_basic :: H.Property
prop_basic = H.property $ do
  P.take 40 testOutput H.=== expected

unitTests :: TestTree
unitTests = $(testGroupGenerator)
