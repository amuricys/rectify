import Lean.Data.Json
import SciLean

namespace Rectify

open SciLean Lean

set_default_scalar Float

structure LorenzState where
  x : Float
  y : Float
  z : Float
deriving Repr, ToJson

structure LorenzParams where
  σ : Float
  ρ : Float
  β : Float
  deriving Repr, ToJson

def lorenzSystem (params : LorenzParams) (x y z : Float) : Float × Float × Float :=
  ( params.σ * (y - x),
    x * (params.ρ - z) - y,
    x * y - params.β * z )

approx lorenzSolver (params : LorenzParams)
  := odeSolve (fun (t : Float) (x,y,z) =>
      let (dx, dy, dz) := lorenzSystem params x y z
      (dx, dy, dz))
by
  unfold lorenzSystem
  simp_rw (config:={zeta:=false}) [odeSolve_fixed_dt rungeKutta4 sorry_proof]
  approx_limit n sorry_proof

def lorenzSteps (params : LorenzParams) (state : LorenzState) (t₀ t : Float) (substeps : Nat) : LorenzState :=
  let (x, y, z) := lorenzSolver params substeps t₀ t (state.x, state.y, state.z)
  { state with x := x, y := y, z := z }
