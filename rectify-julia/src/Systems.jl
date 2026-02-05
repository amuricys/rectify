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

Expanded input port convention:
  - Ports 1..nstates: state replacement inputs (NaN = not connected, own dynamics used)
  - Ports nstates+1..nstates+nparams: parameter inputs (default: parameter's configured value)

When a state input is wired, that variable's derivative is set to 0 and its value
is overridden by the incoming signal after each step. The other equations still
reference it, but it is algebraically constrained. (Additive driving can be derived
from this by wiring through an adder system.)
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
    input_defaults::Vector{Float64}
    constructor::Function  # (params::Dict) -> (ContinuousMachine, initial_state)
end

"""
Global registry of available system templates
"""
const SYSTEM_REGISTRY = Dict{String, SystemTemplate}()

# =============================================================================
# Lorenz Attractor
# =============================================================================

"""
    lorenz_machine(; σ=10.0, ρ=28.0, β=8/3)

Classic Lorenz attractor as an open system with expanded inputs.
- 3 states: x, y, z
- 6 inputs: x_in, y_in, z_in (state replacement), σ, ρ, β (parameter)
- 3 outputs: x, y, z
"""
function lorenz_machine(; σ=10.0, ρ=28.0, β=8/3)
    function dynamics(u, x, p, t)
        x_state, y_state, z_state = u
        # State replacement inputs: NaN means use own state
        x_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : x_state
        y_eff = (length(x) >= 2 && !isnan(x[2])) ? x[2] : y_state
        z_eff = (length(x) >= 3 && !isnan(x[3])) ? x[3] : z_state
        # Parameter inputs
        σ_eff = (length(x) >= 4 && !isnan(x[4])) ? x[4] : σ
        ρ_eff = (length(x) >= 5 && !isnan(x[5])) ? x[5] : ρ
        β_eff = (length(x) >= 6 && !isnan(x[6])) ? x[6] : β

        dx = (length(x) >= 1 && !isnan(x[1])) ? 0.0 : σ_eff * (y_eff - x_eff)
        dy = (length(x) >= 2 && !isnan(x[2])) ? 0.0 : x_eff * (ρ_eff - z_eff) - y_eff
        dz = (length(x) >= 3 && !isnan(x[3])) ? 0.0 : x_eff * y_eff - β_eff * z_eff

        return [dx, dy, dz]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(6, 3, 3, dynamics, readout)
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
    3, 6, 3,
    [("sigma", 10.0), ("rho", 28.0), ("beta", 8/3)],
    ["x", "y", "z"],
    ["x_in", "y_in", "z_in", "σ", "ρ", "β"],
    ["x", "y", "z"],
    [NaN, NaN, NaN, 10.0, 28.0, 8/3],
    lorenz_constructor
)

# =============================================================================
# Rössler Attractor
# =============================================================================

function rossler_machine(; a=0.2, b=0.2, c=5.7)
    function dynamics(u, x, p, t)
        x_state, y_state, z_state = u
        x_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : x_state
        y_eff = (length(x) >= 2 && !isnan(x[2])) ? x[2] : y_state
        z_eff = (length(x) >= 3 && !isnan(x[3])) ? x[3] : z_state
        a_eff = (length(x) >= 4 && !isnan(x[4])) ? x[4] : a
        b_eff = (length(x) >= 5 && !isnan(x[5])) ? x[5] : b
        c_eff = (length(x) >= 6 && !isnan(x[6])) ? x[6] : c

        dx = (length(x) >= 1 && !isnan(x[1])) ? 0.0 : -y_eff - z_eff
        dy = (length(x) >= 2 && !isnan(x[2])) ? 0.0 : x_eff + a_eff * y_eff
        dz = (length(x) >= 3 && !isnan(x[3])) ? 0.0 : b_eff + z_eff * (x_eff - c_eff)

        return [dx, dy, dz]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(6, 3, 3, dynamics, readout)
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
    3, 6, 3,
    [("a", 0.2), ("b", 0.2), ("c", 5.7)],
    ["x", "y", "z"],
    ["x_in", "y_in", "z_in", "a", "b", "c"],
    ["x", "y", "z"],
    [NaN, NaN, NaN, 0.2, 0.2, 5.7],
    rossler_constructor
)

# =============================================================================
# Chen Attractor
# =============================================================================

function chen_machine(; a=35.0, b=3.0, c=28.0)
    function dynamics(u, x, p, t)
        x_state, y_state, z_state = u
        x_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : x_state
        y_eff = (length(x) >= 2 && !isnan(x[2])) ? x[2] : y_state
        z_eff = (length(x) >= 3 && !isnan(x[3])) ? x[3] : z_state
        a_eff = (length(x) >= 4 && !isnan(x[4])) ? x[4] : a
        b_eff = (length(x) >= 5 && !isnan(x[5])) ? x[5] : b
        c_eff = (length(x) >= 6 && !isnan(x[6])) ? x[6] : c

        dx = (length(x) >= 1 && !isnan(x[1])) ? 0.0 : a_eff * (y_eff - x_eff)
        dy = (length(x) >= 2 && !isnan(x[2])) ? 0.0 : (c_eff - a_eff) * x_eff - x_eff * z_eff + c_eff * y_eff
        dz = (length(x) >= 3 && !isnan(x[3])) ? 0.0 : x_eff * y_eff - b_eff * z_eff

        return [dx, dy, dz]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(6, 3, 3, dynamics, readout)
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
    3, 6, 3,
    [("a", 35.0), ("b", 3.0), ("c", 28.0)],
    ["x", "y", "z"],
    ["x_in", "y_in", "z_in", "a", "b", "c"],
    ["x", "y", "z"],
    [NaN, NaN, NaN, 35.0, 3.0, 28.0],
    chen_constructor
)

# =============================================================================
# Van der Pol Oscillator
# =============================================================================

function vanderpol_machine(; μ=1.0)
    function dynamics(u, x, p, t)
        x_state, y_state = u
        x_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : x_state
        y_eff = (length(x) >= 2 && !isnan(x[2])) ? x[2] : y_state
        μ_eff = (length(x) >= 3 && !isnan(x[3])) ? x[3] : μ

        dx = (length(x) >= 1 && !isnan(x[1])) ? 0.0 : y_eff
        dy = (length(x) >= 2 && !isnan(x[2])) ? 0.0 : μ_eff * (1 - x_eff^2) * y_eff - x_eff

        return [dx, dy]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(3, 2, 2, dynamics, readout)
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
    2, 3, 2,
    [("mu", 1.0)],
    ["x", "y"],
    ["x_in", "y_in", "μ"],
    ["x", "y"],
    [NaN, NaN, 1.0],
    vanderpol_constructor
)

# =============================================================================
# Harmonic Oscillator
# =============================================================================

function harmonic_machine(; m=1.0, k=1.0, damping=0.1)
    function dynamics(u, x, p, t)
        pos, vel = u
        pos_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : pos
        vel_eff = (length(x) >= 2 && !isnan(x[2])) ? x[2] : vel
        m_eff = (length(x) >= 3 && !isnan(x[3])) ? x[3] : m
        k_eff = (length(x) >= 4 && !isnan(x[4])) ? x[4] : k
        damp_eff = (length(x) >= 5 && !isnan(x[5])) ? x[5] : damping

        dpos = (length(x) >= 1 && !isnan(x[1])) ? 0.0 : vel_eff
        dvel = (length(x) >= 2 && !isnan(x[2])) ? 0.0 : (-k_eff * pos_eff - damp_eff * vel_eff) / m_eff

        return [dpos, dvel]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(5, 2, 2, dynamics, readout)
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
    2, 5, 2,
    [("m", 1.0), ("k", 1.0), ("damping", 0.1)],
    ["position", "velocity"],
    ["pos_in", "vel_in", "m", "k", "damping"],
    ["position", "velocity"],
    [NaN, NaN, 1.0, 1.0, 0.1],
    harmonic_constructor
)

# =============================================================================
# Duffing Oscillator
# =============================================================================

function duffing_machine(; δ=0.3, α=-1.0, β=1.0, γ=0.5, ω=1.2)
    function dynamics(u, x, p, t)
        pos, vel = u
        pos_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : pos
        vel_eff = (length(x) >= 2 && !isnan(x[2])) ? x[2] : vel
        δ_eff = (length(x) >= 3 && !isnan(x[3])) ? x[3] : δ
        α_eff = (length(x) >= 4 && !isnan(x[4])) ? x[4] : α
        β_eff = (length(x) >= 5 && !isnan(x[5])) ? x[5] : β
        γ_eff = (length(x) >= 6 && !isnan(x[6])) ? x[6] : γ
        ω_eff = (length(x) >= 7 && !isnan(x[7])) ? x[7] : ω

        drive = γ_eff * cos(ω_eff * t)

        dpos = (length(x) >= 1 && !isnan(x[1])) ? 0.0 : vel_eff
        dvel = (length(x) >= 2 && !isnan(x[2])) ? 0.0 : -δ_eff * vel_eff - α_eff * pos_eff - β_eff * pos_eff^3 + drive

        return [dpos, dvel]
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(7, 2, 2, dynamics, readout)
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
    2, 7, 2,
    [("delta", 0.3), ("alpha", -1.0), ("beta", 1.0), ("gamma", 0.5), ("omega", 1.2)],
    ["position", "velocity"],
    ["pos_in", "vel_in", "δ", "α", "β", "γ", "ω"],
    ["position", "velocity"],
    [NaN, NaN, 0.3, -1.0, 1.0, 0.5, 1.2],
    duffing_constructor
)

# =============================================================================
# Constant Source
# =============================================================================

function constant_machine(; value=1.0)
    dynamics(u, x, p, t) = [0.0]
    readout(u, p, t) = u

    ContinuousMachine{Float64}(1, 1, 1, dynamics, readout)
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
    1, 1, 1,
    [("value", 1.0)],
    ["value"],
    ["val_in"],
    ["out"],
    [NaN],
    constant_constructor
)

# =============================================================================
# Sine Wave Generator
# =============================================================================

function sine_machine(; amplitude=1.0, frequency=1.0, phase=0.0)
    ω = 2π * frequency

    function dynamics(u, x, p, t)
        y, dydt = u
        y_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : y
        dydt_eff = (length(x) >= 2 && !isnan(x[2])) ? x[2] : dydt
        amp_eff = (length(x) >= 3 && !isnan(x[3])) ? x[3] : amplitude
        freq_eff = (length(x) >= 4 && !isnan(x[4])) ? x[4] : frequency
        ω_eff = 2π * freq_eff

        dy = (length(x) >= 1 && !isnan(x[1])) ? 0.0 : dydt_eff
        ddydt = (length(x) >= 2 && !isnan(x[2])) ? 0.0 : -ω_eff^2 * y_eff

        return [dy, ddydt]
    end

    readout(u, p, t) = [amplitude * u[1]]

    ContinuousMachine{Float64}(4, 2, 1, dynamics, readout)
end

function sine_constructor(params::Dict{String, Float64})
    amplitude = get(params, "amplitude", 1.0)
    frequency = get(params, "frequency", 1.0)
    phase = get(params, "phase", 0.0)
    machine = sine_machine(; amplitude=amplitude, frequency=frequency, phase=phase)
    ω = 2π * frequency
    initial_state = [sin(phase), ω * cos(phase)]
    return machine, initial_state
end

SYSTEM_REGISTRY["sine"] = SystemTemplate(
    "sine",
    "Sine Wave Generator",
    2, 4, 1,
    [("amplitude", 1.0), ("frequency", 1.0)],
    ["y", "dy/dt"],
    ["y_in", "dydt_in", "amplitude", "frequency"],
    ["signal"],
    [NaN, NaN, 1.0, 1.0],
    sine_constructor
)

# =============================================================================
# Linear Scaler/Gain
# =============================================================================

function scaler_machine(; gain=1.0)
    function dynamics(u, x, p, t)
        val = u[1]
        val_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : val
        gain_eff = (length(x) >= 2 && !isnan(x[2])) ? x[2] : gain

        # State replacement: if val_in is driven, derivative is 0
        if length(x) >= 1 && !isnan(x[1])
            return [0.0]
        else
            # Track gain * input rapidly
            target = gain_eff * (length(x) >= 1 ? 0.0 : 0.0)
            return [gain_eff * (length(x) >= 1 && !isnan(x[1]) ? x[1] : 0.0) - val]
        end
    end

    readout(u, p, t) = u

    ContinuousMachine{Float64}(2, 1, 1, dynamics, readout)
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
    1, 2, 1,
    [("gain", 1.0)],
    ["value"],
    ["in", "gain"],
    ["out"],
    [NaN, 1.0],
    scaler_constructor
)

# =============================================================================
# Integrator
# =============================================================================

function integrator_machine()
    function dynamics(u, x, p, t)
        val = u[1]
        val_eff = (length(x) >= 1 && !isnan(x[1])) ? x[1] : val

        if length(x) >= 1 && !isnan(x[1])
            return [0.0]  # Driven — derivative is 0
        else
            # Integrate signal from port 1 (same port, but when it's NaN we have no input)
            return [0.0]
        end
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
    [NaN],
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
        "output_names" => t.output_names,
        "input_defaults" => [isnan(v) ? nothing : v for v in t.input_defaults]
    ) for t in values(SYSTEM_REGISTRY)]
end

end # module
