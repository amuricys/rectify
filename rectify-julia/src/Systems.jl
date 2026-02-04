module Systems

using AlgebraicDynamics.DWDDynam
using Catlab.WiringDiagrams

export SystemTemplate, SYSTEM_REGISTRY
export lorenz_machine, harmonic_machine, vanderpol_machine, duffing_machine
export constant_machine, rossler_machine, chen_machine
export create_system, list_templates

# =============================================================================
# System Template Registry
# =============================================================================

"""
    SystemTemplate

Metadata about a dynamical system type, including how to construct it.
"""
struct SystemTemplate
    id::String
    name::String
    nstates::Int
    ninputs::Int
    noutputs::Int
    parameters::Vector{Tuple{String, Float64}}  # (name, default)
    state_names::Vector{String}
    input_names::Vector{String}
    output_names::Vector{String}
    constructor::Function  # (params::Dict) -> (ContinuousMachine, initial_state)
end

"""
Global registry of available system templates
"""
const SYSTEM_REGISTRY = Dict{String, SystemTemplate}()

# Note: ninputs, nstates, noutputs are functions from AlgebraicDynamics.DWDDynam
# They are re-exported for convenience

# =============================================================================
# Lorenz Attractor
# =============================================================================

"""
    lorenz_machine(; σ=10.0, ρ=28.0, β=8/3)

Classic Lorenz attractor as an open system.
- 3 states: x, y, z
- 1 input: additive perturbation to ρ parameter
- 3 outputs: x, y, z (full state exposed)

The famous chaotic attractor with butterfly-shaped trajectory.
"""
function lorenz_machine(; σ=10.0, ρ=28.0, β=8/3)
    function dynamics(u, x, p, t)
        # u is the state vector [x, y, z]
        # x is the input vector
        x_state, y_state, z_state = u
        ρ_eff = ρ + (length(x) > 0 ? x[1] : 0.0)

        dx = σ * (y_state - x_state)
        dy = x_state * (ρ_eff - z_state) - y_state
        dz = x_state * y_state - β * z_state

        return [dx, dy, dz]
    end

    readout(u, p, t) = u  # Full state as output

    ContinuousMachine{Float64}(1, 3, 3, dynamics, readout)
end

function lorenz_constructor(params::Dict{String, Float64})
    σ = get(params, "sigma", 10.0)
    ρ = get(params, "rho", 28.0)
    β = get(params, "beta", 8/3)
    machine = lorenz_machine(; σ=σ, ρ=ρ, β=β)
    initial_state = [1.0, 1.0, 1.0]
    return machine, initial_state
end

SYSTEM_REGISTRY["lorenz"] = SystemTemplate(
    "lorenz",
    "Lorenz Attractor",
    3, 1, 3,
    [("sigma", 10.0), ("rho", 28.0), ("beta", 8/3)],
    ["x", "y", "z"],
    ["ρ_mod"],
    ["x", "y", "z"],
    lorenz_constructor
)

# =============================================================================
# Rössler Attractor
# =============================================================================

"""
    rossler_machine(; a=0.2, b=0.2, c=5.7)

Rössler attractor - simpler chaotic system than Lorenz.
- 3 states: x, y, z
- 1 input: additive perturbation to y equation
- 3 outputs: x, y, z
"""
function rossler_machine(; a=0.2, b=0.2, c=5.7)
    function dynamics(u, x, p, t)
        x_state, y_state, z_state = u
        drive = length(x) > 0 ? x[1] : 0.0

        dx = -y_state - z_state
        dy = x_state + a * y_state + drive
        dz = b + z_state * (x_state - c)

        return [dx, dy, dz]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 3, 3, dynamics, readout)
end

function rossler_constructor(params::Dict{String, Float64})
    a = get(params, "a", 0.2)
    b = get(params, "b", 0.2)
    c = get(params, "c", 5.7)
    machine = rossler_machine(; a=a, b=b, c=c)
    initial_state = [1.0, 1.0, 1.0]
    return machine, initial_state
end

SYSTEM_REGISTRY["rossler"] = SystemTemplate(
    "rossler",
    "Rössler Attractor",
    3, 1, 3,
    [("a", 0.2), ("b", 0.2), ("c", 5.7)],
    ["x", "y", "z"],
    ["drive"],
    ["x", "y", "z"],
    rossler_constructor
)

# =============================================================================
# Chen Attractor
# =============================================================================

"""
    chen_machine(; a=35.0, b=3.0, c=28.0)

Chen attractor - another 3D chaotic system.
"""
function chen_machine(; a=35.0, b=3.0, c=28.0)
    function dynamics(u, x, p, t)
        x_state, y_state, z_state = u
        drive = length(x) > 0 ? x[1] : 0.0

        dx = a * (y_state - x_state)
        dy = (c - a) * x_state - x_state * z_state + c * y_state + drive
        dz = x_state * y_state - b * z_state

        return [dx, dy, dz]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 3, 3, dynamics, readout)
end

function chen_constructor(params::Dict{String, Float64})
    a = get(params, "a", 35.0)
    b = get(params, "b", 3.0)
    c = get(params, "c", 28.0)
    machine = chen_machine(; a=a, b=b, c=c)
    initial_state = [-0.1, 0.5, -0.6]
    return machine, initial_state
end

SYSTEM_REGISTRY["chen"] = SystemTemplate(
    "chen",
    "Chen Attractor",
    3, 1, 3,
    [("a", 35.0), ("b", 3.0), ("c", 28.0)],
    ["x", "y", "z"],
    ["drive"],
    ["x", "y", "z"],
    chen_constructor
)

# =============================================================================
# Van der Pol Oscillator
# =============================================================================

"""
    vanderpol_machine(; μ=1.0)

Van der Pol oscillator - self-sustaining nonlinear oscillator.
- 2 states: position x, velocity y
- 1 input: external drive
- 2 outputs: x, y
"""
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

function vanderpol_constructor(params::Dict{String, Float64})
    μ = get(params, "mu", 1.0)
    machine = vanderpol_machine(; μ=μ)
    initial_state = [2.0, 0.0]
    return machine, initial_state
end

SYSTEM_REGISTRY["vanderpol"] = SystemTemplate(
    "vanderpol",
    "Van der Pol Oscillator",
    2, 1, 2,
    [("mu", 1.0)],
    ["x", "y"],
    ["drive"],
    ["x", "y"],
    vanderpol_constructor
)

# =============================================================================
# Harmonic Oscillator
# =============================================================================

"""
    harmonic_machine(; m=1.0, k=1.0, damping=0.1)

Damped harmonic oscillator.
- 2 states: position, velocity
- 1 input: external force
- 2 outputs: position, velocity
"""
function harmonic_machine(; m=1.0, k=1.0, damping=0.1)
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

function harmonic_constructor(params::Dict{String, Float64})
    m = get(params, "m", 1.0)
    k = get(params, "k", 1.0)
    damping = get(params, "damping", 0.1)
    machine = harmonic_machine(; m=m, k=k, damping=damping)
    initial_state = [1.0, 0.0]
    return machine, initial_state
end

SYSTEM_REGISTRY["harmonic"] = SystemTemplate(
    "harmonic",
    "Harmonic Oscillator",
    2, 1, 2,
    [("m", 1.0), ("k", 1.0), ("damping", 0.1)],
    ["position", "velocity"],
    ["force"],
    ["position", "velocity"],
    harmonic_constructor
)

# =============================================================================
# Duffing Oscillator
# =============================================================================

"""
    duffing_machine(; δ=0.3, α=-1.0, β=1.0, γ=0.5, ω=1.2)

Duffing oscillator - driven nonlinear oscillator with cubic stiffness.
Can exhibit chaotic behavior for certain parameter values.
"""
function duffing_machine(; δ=0.3, α=-1.0, β=1.0, γ=0.5, ω=1.2)
    function dynamics(u, x, p, t)
        pos, vel = u
        external = length(x) > 0 ? x[1] : 0.0

        # Internal periodic drive + external input
        drive = γ * cos(ω * t) + external

        dpos = vel
        dvel = -δ * vel - α * pos - β * pos^3 + drive

        return [dpos, dvel]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 2, 2, dynamics, readout)
end

function duffing_constructor(params::Dict{String, Float64})
    δ = get(params, "delta", 0.3)
    α = get(params, "alpha", -1.0)
    β = get(params, "beta", 1.0)
    γ = get(params, "gamma", 0.5)
    ω = get(params, "omega", 1.2)
    machine = duffing_machine(; δ=δ, α=α, β=β, γ=γ, ω=ω)
    initial_state = [1.0, 0.0]
    return machine, initial_state
end

SYSTEM_REGISTRY["duffing"] = SystemTemplate(
    "duffing",
    "Duffing Oscillator",
    2, 1, 2,
    [("delta", 0.3), ("alpha", -1.0), ("beta", 1.0), ("gamma", 0.5), ("omega", 1.2)],
    ["position", "velocity"],
    ["external"],
    ["position", "velocity"],
    duffing_constructor
)

# =============================================================================
# Constant Source (Parameter/Signal Source)
# =============================================================================

"""
    constant_machine(; value=1.0)

Constant value source - outputs a fixed value.
Useful for providing parameter inputs to other systems.
"""
function constant_machine(; value=1.0)
    dynamics(u, x, p, t) = [0.0]  # No change
    readout(u, p, t) = u  # Output is the state

    ContinuousMachine{Float64}(0, 1, 1, dynamics, readout)
end

function constant_constructor(params::Dict{String, Float64})
    value = get(params, "value", 1.0)
    machine = constant_machine(; value=value)
    initial_state = [value]
    return machine, initial_state
end

SYSTEM_REGISTRY["constant"] = SystemTemplate(
    "constant",
    "Constant Source",
    1, 0, 1,
    [("value", 1.0)],
    ["value"],
    String[],
    ["out"],
    constant_constructor
)

# =============================================================================
# Sine Wave Generator
# =============================================================================

"""
    sine_machine(; amplitude=1.0, frequency=1.0, phase=0.0)

Sine wave generator implemented as a harmonic oscillator.
Outputs: sin(ωt + φ) where ω = 2π * frequency
"""
function sine_machine(; amplitude=1.0, frequency=1.0, phase=0.0)
    ω = 2π * frequency

    function dynamics(u, x, p, t)
        # Harmonic oscillator: d²y/dt² = -ω²y
        # State: [y, dy/dt]
        y, dydt = u
        return [dydt, -ω^2 * y]
    end

    readout(u, p, t) = [amplitude * u[1]]

    ContinuousMachine{Float64}(0, 2, 1, dynamics, readout)
end

function sine_constructor(params::Dict{String, Float64})
    amplitude = get(params, "amplitude", 1.0)
    frequency = get(params, "frequency", 1.0)
    phase = get(params, "phase", 0.0)
    machine = sine_machine(; amplitude=amplitude, frequency=frequency, phase=phase)
    # Initial conditions for sin with phase
    ω = 2π * frequency
    initial_state = [sin(phase), ω * cos(phase)]
    return machine, initial_state
end

SYSTEM_REGISTRY["sine"] = SystemTemplate(
    "sine",
    "Sine Wave Generator",
    2, 0, 1,
    [("amplitude", 1.0), ("frequency", 1.0), ("phase", 0.0)],
    ["y", "dy/dt"],
    String[],
    ["signal"],
    sine_constructor
)

# =============================================================================
# Linear Scaler/Gain
# =============================================================================

"""
    scaler_machine(; gain=1.0)

Linear gain block - multiplies input by a constant.
"""
function scaler_machine(; gain=1.0)
    # Stateless in effect, but AlgebraicDynamics requires state
    # We use a single state that tracks the scaled input
    function dynamics(u, x, p, t)
        if length(x) > 0
            return [gain * x[1] - u[1]]  # Rapidly track input
        else
            return [0.0]
        end
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 1, 1, dynamics, readout)
end

function scaler_constructor(params::Dict{String, Float64})
    gain = get(params, "gain", 1.0)
    machine = scaler_machine(; gain=gain)
    initial_state = [0.0]
    return machine, initial_state
end

SYSTEM_REGISTRY["scaler"] = SystemTemplate(
    "scaler",
    "Linear Scaler",
    1, 1, 1,
    [("gain", 1.0)],
    ["value"],
    ["in"],
    ["out"],
    scaler_constructor
)

# =============================================================================
# Integrator
# =============================================================================

"""
    integrator_machine()

Pure integrator - state is the integral of the input.
"""
function integrator_machine()
    function dynamics(u, x, p, t)
        input = length(x) > 0 ? x[1] : 0.0
        return [input]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 1, 1, dynamics, readout)
end

function integrator_constructor(params::Dict{String, Float64})
    machine = integrator_machine()
    initial_value = get(params, "initial", 0.0)
    initial_state = [initial_value]
    return machine, initial_state
end

SYSTEM_REGISTRY["integrator"] = SystemTemplate(
    "integrator",
    "Integrator",
    1, 1, 1,
    [("initial", 0.0)],
    ["integral"],
    ["in"],
    ["out"],
    integrator_constructor
)

# =============================================================================
# Utility Functions
# =============================================================================

"""
    create_system(template_id::String, params::Dict{String, Float64})

Create a system instance from a registered template.
Returns (machine::ContinuousMachine, initial_state::Vector{Float64})
"""
function create_system(template_id::String, params::Dict{String, Float64})
    if !haskey(SYSTEM_REGISTRY, template_id)
        error("Unknown system template: $template_id. Available: $(keys(SYSTEM_REGISTRY))")
    end
    template = SYSTEM_REGISTRY[template_id]
    return template.constructor(params)
end

"""
    list_templates()

List all available system templates.
"""
function list_templates()
    [Dict(
        "id" => t.id,
        "name" => t.name,
        "nstates" => t.nstates,
        "ninputs" => t.ninputs,
        "noutputs" => t.noutputs,
        "parameters" => [Dict("name" => p[1], "default" => p[2]) for p in t.parameters],
        "state_names" => t.state_names,
        "input_names" => t.input_names,
        "output_names" => t.output_names
    ) for t in values(SYSTEM_REGISTRY)]
end

end # module
