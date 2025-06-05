import Lean.Data.Json
import SciLean

namespace Rectify

open SciLean Lean

set_default_scalar Float

structure DoublePendulumState where
  θ₁ : Float
  θ₂ : Float
  ω₁ : Float
  ω₂ : Float
  deriving Repr, ToJson

structure DoublePendulumParams where
  m : Float
  l : Float
  deriving Repr, ToJson

-- Double Pendulum (using small angle approximation for now)
def doublePendulumEnergy (m l : Float) (θ₁ θ₂ p₁ p₂ : Float) : Float :=
  let T := (1/(2*m*l^2)) * (p₁^2 + p₂^2)
  let V := m * 9.81 * l * (2 - Float.cos θ₁ - Float.cos θ₂)
  T + V

noncomputable approx doublePendulumSolver (m l : Float)
  := odeSolve (fun (t : Float) (θ₁, θ₂, p₁, p₂) =>
      ( ∇ (p₁':=p₁), doublePendulumEnergy m l θ₁ θ₂ p₁' p₂,
        ∇ (θ₁':=θ₁), doublePendulumEnergy m l θ₁' θ₂ p₁ p₂,
        ∇ (p₂':=p₂), doublePendulumEnergy m l θ₁ θ₂ p₁ p₂',
        ∇ (θ₂':=θ₁), doublePendulumEnergy m l θ₁ θ₂' p₁ p₂
        ))
by
  unfold doublePendulumEnergy
  autodiff
  simp_rw (config:={zeta:=false}) [odeSolve_fixed_dt rungeKutta4 sorry_proof]
  approx_limit n sorry_proof

noncomputable def doublePendulumSteps (params : DoublePendulumParams) (state : DoublePendulumState) (t₀ t : Float) (substeps : Nat) : DoublePendulumState :=
  let (θ₁, θ₂, ω₁, ω₂) := doublePendulumSolver params.m params.l substeps t₀ t (state.θ₁, state.θ₂, state.ω₁, state.ω₂)
  { state with θ₁ := θ₁, θ₂ := θ₂, ω₁ := ω₁, ω₂ := ω₂ }
