module Simulation

using AlgebraicDynamics.DWDDynam: eval_dynamics, readout, ninputs

export step!, step_composed!, euler_step, rk4_step
export step_world!, step_world_independent!

# =============================================================================
# Integration Methods
# =============================================================================

"""
    euler_step(f, u, t, dt)

Single Euler integration step.
Simple but only first-order accurate.
"""
function euler_step(f, u::Vector{Float64}, t::Float64, dt::Float64)
    du = f(u, t)
    return u .+ dt .* du
end

"""
    rk4_step(f, u, t, dt)

Single RK4 (Runge-Kutta 4th order) integration step.
Fourth-order accurate, much better for chaotic systems.
"""
function rk4_step(f, u::Vector{Float64}, t::Float64, dt::Float64)
    k1 = f(u, t)
    k2 = f(u .+ 0.5 * dt .* k1, t + 0.5 * dt)
    k3 = f(u .+ 0.5 * dt .* k2, t + 0.5 * dt)
    k4 = f(u .+ dt .* k3, t + dt)
    return u .+ (dt / 6.0) .* (k1 .+ 2.0 .* k2 .+ 2.0 .* k3 .+ k4)
end

# =============================================================================
# System Stepping (Individual Systems)
# =============================================================================

"""
    step!(sys, inputs, t, dt; method=:rk4)

Advance a single system by one time step.

Arguments:
- sys: SystemInstance with .machine and .state fields
- inputs: Vector of input values
- t: Current time
- dt: Time step
- method: Integration method (:euler or :rk4)

Modifies sys.state in place.
"""
function step!(sys, inputs::Vector{Float64}, t::Float64, dt::Float64; method::Symbol=:rk4)
    # Create dynamics function closed over inputs
    f = (u, t_) -> eval_dynamics(sys.machine, u, inputs, nothing, t_)

    if method == :euler
        sys.state .= euler_step(f, sys.state, t, dt)
    elseif method == :rk4
        sys.state .= rk4_step(f, sys.state, t, dt)
    else
        error("Unknown integration method: $method")
    end

    return sys.state
end

# =============================================================================
# Composed System Stepping
# =============================================================================

"""
    step_composed!(composed_info, composed_state, external_inputs, t, dt; method=:rk4)

Advance a composed system by one time step.

The composed machine from oapply has its own dynamics that already handles
the internal wiring. We just need to provide external inputs and integrate.

Arguments:
- composed_info: ComposedSystemInfo from compose_systems
- composed_state: Current state vector (modified in place)
- external_inputs: Inputs for unconnected input ports
- t: Current time
- dt: Time step
- method: Integration method

Returns the new state (also modified in place).
"""
function step_composed!(
    composed_info,
    composed_state::Vector{Float64},
    external_inputs::Vector{Float64},
    t::Float64,
    dt::Float64;
    method::Symbol=:rk4
)
    machine = composed_info.machine

    # Create dynamics function
    f = (u, t_) -> eval_dynamics(machine, u, external_inputs, nothing, t_)

    if method == :euler
        composed_state .= euler_step(f, composed_state, t, dt)
    elseif method == :rk4
        composed_state .= rk4_step(f, composed_state, t, dt)
    else
        error("Unknown integration method: $method")
    end

    return composed_state
end

# =============================================================================
# World Stepping (Handles Composition)
# =============================================================================

"""
    step_world!(world, dt; method=:rk4)

Advance the entire world by one time step.

If systems are composed, uses the composed dynamics.
Otherwise, steps each system independently while respecting wiring.

This function:
1. Checks if recomposition is needed
2. Steps the composed system (or individual systems)
3. Distributes state back to individual systems
4. Advances world time
"""
function step_world!(world, composed_info, dt::Float64; method::Symbol=:rk4)
    if composed_info === nothing || isempty(world.systems)
        # No systems to simulate
        world.time += dt
        return
    end

    # Extract current composed state
    composed_state = zeros(composed_info.total_states)
    for sys_id in composed_info.system_order
        if haskey(world.systems, sys_id)
            range = composed_info.system_ranges[sys_id]
            composed_state[range] .= world.systems[sys_id].state
        end
    end

    # External inputs (for unconnected input ports)
    # For now, all external inputs are 0
    n_external = ninputs(composed_info.machine)
    external_inputs = zeros(n_external)

    # Step the composed system
    step_composed!(composed_info, composed_state, external_inputs, world.time, dt; method=method)

    # Distribute state back to individual systems
    for sys_id in composed_info.system_order
        if haskey(world.systems, sys_id)
            range = composed_info.system_ranges[sys_id]
            world.systems[sys_id].state .= composed_state[range]
        end
    end

    # Advance time
    world.time += dt
end

# =============================================================================
# Fallback: Independent System Stepping with Manual Wiring
# =============================================================================

"""
    step_world_independent!(world, dt; method=:rk4)

Step systems independently, manually gathering inputs from wires.

This is a fallback for when composition isn't available or for debugging.
Less efficient than proper composition but easier to understand.
"""
function step_world_independent!(world, dt::Float64; method::Symbol=:rk4)
    # First compute all outputs (needed for wiring)
    outputs = Dict{String, Vector{Float64}}()
    for (sys_id, sys) in world.systems
        outputs[sys_id] = readout(sys.machine, sys.state, nothing, world.time)
    end

    # Build wire map for input lookup
    wire_map = Dict{Tuple{String, Int}, Tuple{String, Int}}()
    for wire in values(world.wires)
        wire_map[(wire.to_system, wire.to_port)] = (wire.from_system, wire.from_port)
    end

    # Step each system
    for (sys_id, sys) in world.systems
        # Build inputs from wires
        inputs = zeros(ninputs(sys.machine))
        for port in 1:ninputs(sys.machine)
            if haskey(wire_map, (sys_id, port))
                from_id, from_port = wire_map[(sys_id, port)]
                if haskey(outputs, from_id) && from_port <= length(outputs[from_id])
                    inputs[port] = outputs[from_id][from_port]
                end
            end
        end

        # Step this system
        step!(sys, inputs, world.time, dt; method=method)
    end

    world.time += dt
end

# =============================================================================
# Adaptive Stepping (Future)
# =============================================================================

#=
For stiff systems or when higher accuracy is needed, we could use
DifferentialEquations.jl with adaptive stepping. The challenge is
that we want real-time streaming, not batch solving.

Approach for adaptive stepping:
1. Use a small fixed outer dt for broadcasting (e.g., 1/60 sec)
2. Internally use adaptive stepping to reach that target time
3. This gives accuracy benefits while maintaining frame rate

Example:
    using DifferentialEquations

    function step_adaptive!(sys, inputs, t, target_t)
        prob = ODEProblem(
            (du, u, p, t) -> du .= eval_dynamics(sys.machine, u, inputs, p, t),
            sys.state,
            (t, target_t)
        )
        sol = solve(prob, Tsit5(), save_everystep=false)
        sys.state .= sol.u[end]
    end

This is left for future implementation.
=#

end # module
