-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Reservoir.Project where

import Clash.Prelude
import Reservoir.Matrix

-- Create a domain with the frequency of your input clock. For this example we used
-- 50 MHz.
createDomain vSystem {vName = "Dom50", vPeriod = hzToPeriod 50e6}

-- | @topEntity@ is Clash@s equivalent of @main@ in other programming languages.
-- Clash will look for it when compiling "Example.Project" and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a @topEntity@
-- must be monomorphic and must use non- recursive types. Or, to put it
-- hand-wavily, a @topEntity@ must be translatable to a static number of wires.
--
-- Top entities must be monomorphic, meaning we have to specify all type variables.
-- In this case, we are using the @Dom50@ domain, which we created with @createDomain@
-- and we are using 8-bit unsigned numbers.

-- | Reservoir size and input dim
type M = 100

type N = 3

-- LIF parameters
bias, vth, du, dv :: Double
bias = 0.0
vth = 5.0
du = 0.1
dv = 0.1

-- | On-chip weight storage. We'll treat these as ROMs.
w_in :: Vec M (Vec N Double)
w_in = undefined

--   $(listToVecTH [[  -- fill these rows from host
-- 0.1, -0.2,  0.5],
-- ...
-- ])

w_res :: Vec M (Vec M Double)
w_res = undefined

--   $(listToVecTH [[ -- same deal: host-generated mask & weights
--    0.0, 1.2, ...
--    ...
--  ]])

-- | State for one neuron (u and v)
data NeuronState = NeuronState {u_val, v_val :: Double}
  deriving (Generic, NFDataX)

-- | Single LIF update across all M neurons
activation :: Vec m NeuronState -> Vec m Double -> Vec m (NeuronState, Bit)
activation = zipWith updateNeuron
  where
    updateNeuron :: NeuronState -> Double -> (NeuronState, Bit)
    updateNeuron (NeuronState uP vP) aIn =
      let uN = uP * (1 - du) + aIn
          vRaw = vP * (1 - dv) + uN + bias
       in if vRaw > vth
            then (NeuronState uN 0, 1)
            else (NeuronState uN vRaw, 0)

-- | Top entity: one reservoir step per call.
--    Input:  un ∈ R^N
--    State:  xPrev ∈ R^(M+1)  (with bias at head)
--    Output: xNext ∈ R^(M+1)
topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Vec N Double) -> -- un
  Signal Dom50 (Vec M Double) -- x_{n+1}
topEntity = exposeClockResetEnable $ mealy (reservoirT w_in w_res) initState
  where
    -- initial state = [1] ++ zeros
    initState :: Vec M (NeuronState, Bit)
    initState = repeat (NeuronState 0 0, 0)

-- | Note: I'm diverging from the paper here by having no bias at all.
-- The paper is just not clear on what the bias' role is and whether it's
-- interpreted as another neuron's signal or not.
reservoirT ::
  forall m n.
  (KnownNat m) =>
  (KnownNat n) =>
  (1 <= m) =>
  (1 <= n) =>
  -- | W_in
  Vec m (Vec n Double) ->
  -- | W_res
  Vec m (Vec m Double) ->
  Vec m (NeuronState, Bit) ->
  Vec n Double ->
  (Vec m (NeuronState, Bit), Vec m Double)
reservoirT wIn wRes xN aIn = (xNext, fromIntegral . snd <$> xNext)
  where
    -- current from input, to each neuron
    inCurr :: Vec m Double
    inCurr = mvMult wIn aIn

    prevNeuronState :: Vec m NeuronState
    prevOutput :: Vec m Bit
    (prevNeuronState, prevOutput) = unzip xN

    -- current reservoir state * weights
    resCurr :: Vec m Double
    resCurr = mvMult wRes (fromIntegral <$> prevOutput)

    -- sum to get total input for each LIF
    totalIn = zipWith (+) inCurr resCurr

    -- run one LIF tick
    xNext = activation prevNeuronState totalIn

-- To specify the names of the ports of our top entity, we create a @Synthesize@ annotation.
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "reservoir",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "DIN"
          ],
        t_output = PortName "DOUT"
      }
  )
  #-}
-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}
