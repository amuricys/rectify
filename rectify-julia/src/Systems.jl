module Systems

using AlgebraicDynamics.DWDDynam
using Catlab.WiringDiagrams

export lorenz_machine, harmonic_machine, constant_machine
export OpenSystem, make_system

# An open Lorenz system
# Inputs: 1 (can drive ρ parameter)
# Outputs: 3 (x, y, z)
# States: 3 (x, y, z)
function lorenz_machine(; σ=10.0, ρ=28.0, β=8/3)
    function dynamics(u, x, p, t)
        x_state, y_state, z_state = u
        ρ_driven = ρ + (length(x) > 0 ? x[1] : 0.0)  # input can modulate ρ

        dx = σ * (y_state - x_state)
        dy = x_state * (ρ_driven - z_state) - y_state
        dz = x_state * y_state - β * z_state

        return [dx, dy, dz]
    end

    readout(u, p, t) = u  # expose all state as output

    ContinuousMachine{Float64}(1, 3, 3, dynamics, readout)
end

# An open harmonic oscillator
# Inputs: 1 (external force)
# Outputs: 2 (position, velocity)
# States: 2 (position, velocity)
function harmonic_machine(; m=1.0, k=1.0, damping=0.0)
    function dynamics(u, x, p, t)
        pos, vel = u
        force = length(x) > 0 ? x[1] : 0.0

        dpos = vel
        dvel = (-k * pos - damping * vel + force) / m

        return [dpos, dvel]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 2, 2, dynamics, readout)
end

# A constant/parameter source
# Inputs: 0
# Outputs: 1 (constant value)
# States: 1 (the value, which can drift)
function constant_machine(; value=1.0)
    dynamics(u, x, p, t) = [0.0]  # constant, no change
    readout(u, p, t) = u

    ContinuousMachine{Float64}(0, 1, 1, dynamics, readout)
end

# Van der Pol oscillator
# Inputs: 1 (external drive)
# Outputs: 2 (x, y)
# States: 2 (x, y)
function vanderpol_machine(; μ=1.0)
    function dynamics(u, x, p, t)
        x_state, y_state = u
        drive = length(x) > 0 ? x[1] : 0.0

        dx = y_state
        dy = μ * (1 - x_state^2) * y_state - x_state + drive

        return [dx, dy]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 2, 2, dynamics, readout)
end

# Duffing oscillator
# Inputs: 1 (can replace/add to periodic drive)
# Outputs: 2 (position, velocity)
# States: 2 (position, velocity)
function duffing_machine(; δ=0.3, α=-1.0, β=1.0, γ=0.5, ω=1.2)
    function dynamics(u, x, p, t)
        pos, vel = u
        external = length(x) > 0 ? x[1] : 0.0

        drive = γ * cos(ω * t) + external

        dpos = vel
        dvel = -δ * vel - α * pos - β * pos^3 + drive

        return [dpos, dvel]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 2, 2, dynamics, readout)
end

# Coupling/scaling machine - takes an input and scales it
# Useful for wiring: output of system A → scaler → input of system B
function scaler_machine(; scale=1.0)
    dynamics(u, x, p, t) = [0.0]  # no internal state change
    readout(u, p, t) = length(u) > 0 ? [scale * u[1]] : [0.0]

    # This is more like a "pass-through" - we need to think about this differently
    # In AlgebraicDynamics, machines have state. For pure signal processing,
    # we might want a different abstraction.
    ContinuousMachine{Float64}(1, 1, 1,
        (u, x, p, t) -> x,  # state follows input
        (u, p, t) -> [scale * u[1]]
    )
end

end # module
