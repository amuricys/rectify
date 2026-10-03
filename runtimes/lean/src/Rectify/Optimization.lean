-- Rectify.Optimization
-- Core optimization framework

import Init.Data.Random
import Lean.Data.Json

namespace Rectify.Optimization

-- ============================================================================
-- Random monad
-- ============================================================================

abbrev RandomM := StateM StdGen

def randFloat : RandomM Float := do
  let gen ← get
  let (n, gen') := stdNext gen
  set gen'
  let (lo, hi) := stdRange
  pure $ (n.toFloat - lo.toFloat) / (hi.toFloat - lo.toFloat + 1.0)

def randNatRange (lo hi : Nat) : RandomM Nat := do
  let gen ← get
  let (n, gen') := randNat gen lo hi
  set gen'
  pure n

def runRandom (seed : Nat) (m : RandomM α) : α :=
  (m.run (mkStdGen seed)).1

def runRandomState (seed : Nat) (m : RandomM α) : α × StdGen :=
  m.run (mkStdGen seed)

-- ============================================================================
-- Problem (pure domain)
-- ============================================================================

/-- A problem defines solution type and fitness only -/
structure Problem (Solution : Type) where
  /-- Compute fitness (lower is better) -/
  fitness : Solution → Float

-- ============================================================================
-- JSON utilities
-- ============================================================================

instance : Lean.ToJson Float where
  toJson f := match Lean.JsonNumber.fromFloat? f with
    | .inr n => Lean.Json.num n
    | .inl _ => Lean.Json.num 0

end Rectify.Optimization
