import SciLean

namespace Rectify.Dynamics

open SciLean

set_default_scalar Float

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

def solveHarmonic (m k : Float) (t₀ t : Float) (x₀ p₀ : Float) (substeps : Nat) : Float × Float :=
  harmonicSolver m k substeps t₀ t (x₀, p₀)

-- Generate trajectory
def harmonicTrajectory (m k : Float) (x₀ p₀ : Float) (dt : Float) (steps : Nat) : Array (Float × Float × Float) := Id.run do
  let mut t := 0.0
  let mut x := x₀
  let mut p := p₀
  let mut trajectory := #[]

  for _ in [0:steps] do
    trajectory := trajectory.push (t, x, p)
    (x, p) := solveHarmonic m k t (t + dt) x p 10
    t := t + dt

  return trajectory

end Rectify.Dynamics
