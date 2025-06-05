import Lean.Data.Json
import SciLean

namespace Rectify

open SciLean Lean

set_default_scalar Float

structure DuffingState where
  x : Float
  v : Float
  deriving Repr, ToJson

structure DuffingParams where
  δ : Float
  α : Float
  β : Float
  γ : Float
  ω : Float
  deriving Repr, ToJson

def duffingSystem (δ α β γ ω : Float) (t x v : Float) : Float × Float :=
  (v, γ * Float.cos (ω * t) - δ * v - α * x - β * x^3)

approx duffingSolver (δ α β γ ω : Float)
  := odeSolve (fun (t : Float) (x,v) => duffingSystem δ α β γ ω t x v)
by
  unfold duffingSystem
  simp_rw (config:={zeta:=false}) [odeSolve_fixed_dt rungeKutta4 sorry_proof]
  approx_limit n sorry_proof

def duffingSteps (params : DuffingParams) (state : DuffingState) (t₀ t : Float) (substeps : Nat) : DuffingState :=
  let (x, v) := duffingSolver params.δ params.α params.β params.γ params.ω substeps t₀ t (state.x, state.v)
  { state with x := x, v := v }
