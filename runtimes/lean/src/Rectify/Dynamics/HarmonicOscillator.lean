import Lean.Data.Json
import SciLean

namespace Rectify

open SciLean Lean

set_default_scalar Float

structure HarmonicOscillatorState where
  x : Float
  p : Float
  deriving Repr, ToJson

structure HarmonicOscillatorParams where
  m : Float
  k : Float
  deriving Repr, ToJson

def H (m k x p : Float) := (1/(2*m)) * p^2 + k/2 * x^2

approx harmonicSolver (m k : Float)
  := odeSolve (fun (t : Float) (x,p) => ( ∇ (p':=p), H m k x  p',
                                         -∇ (x':=x), H m k x' p))
by
  -- Unfold Hamiltonian and compute gradients
  unfold H
  autodiff

  -- Apply RK4 method
  simp_rw (config:={zeta:=false}) [odeSolve_fixed_dt rungeKutta4 sorry_proof]

  -- todo: make approx_limit ignore leading let bindings
  approx_limit n sorry_proof

def harmonicOscillatorSteps (params : HarmonicOscillatorParams) (state : HarmonicOscillatorState) (t₀ t : Float) (substeps : Nat) : HarmonicOscillatorState :=
  let (x, p) := harmonicSolver params.m params.k substeps t₀ t (state.x, state.p)
  { state with x := x, p := p }
