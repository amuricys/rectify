import Lean.Data.Json
import SciLean

namespace Rectify

open SciLean Lean

set_default_scalar Float


structure VanDerPolState where
  x : Float
  y : Float
  deriving Repr, ToJson

structure VanDerPolParams where
  μ : Float
  deriving Repr, ToJson

def vanDerPolSystem (μ : Float) (x v : Float) : Float × Float :=
  (v, μ * (1 - x^2) * v - x)

approx vanDerPolSolver (μ : Float)
  := odeSolve (fun (t : Float) (x,v) => vanDerPolSystem μ x v)
by
  unfold vanDerPolSystem
  simp_rw (config:={zeta:=false}) [odeSolve_fixed_dt rungeKutta4 sorry_proof]
  approx_limit n sorry_proof

def vanDerPolSteps (params : VanDerPolParams) (state : VanDerPolState) (t₀ t : Float) (substeps : Nat) : VanDerPolState :=
  let (x, v) := vanDerPolSolver params.μ substeps t₀ t (state.x, state.y)
  { state with x := x, y := v }
