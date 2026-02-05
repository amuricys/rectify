module World

using AlgebraicDynamics.DWDDynam: ContinuousMachine, readout, ninputs, noutputs, nstates, eval_dynamics

export WorldState, SystemInstance, WireSpec
export add_system!, remove_system!, add_wire!, remove_wire!
export get_system, list_systems, list_wires
export reset_world!, set_system_state!, set_system_params!
export record_history!, serialize_world, serialize_state_update

# =============================================================================
# Core Types
# =============================================================================

"""
    SystemInstance

A live instance of a dynamical system with current state.
"""
mutable struct SystemInstance
    id::String
    template_id::String
    machine::ContinuousMachine{Float64}
    state::Vector{Float64}
    parameters::Dict{String, Float64}
    position::Tuple{Float64, Float64}  # UI position for visualization
end

"""
    WireSpec

Specification for a wire connecting two systems.
"""
struct WireSpec
    id::String
    from_system::String
    from_port::Int  # 1-indexed output port
    to_system::String
    to_port::Int    # 1-indexed input port
end

"""
    WorldState

The complete state of the simulation world.
Contains all system instances, wiring information, and simulation parameters.
"""
mutable struct WorldState
    # System instances by ID
    systems::Dict{String, SystemInstance}

    # Wiring connections by wire ID
    wires::Dict{String, WireSpec}

    # Simulation state
    time::Float64
    running::Bool
    speed::Float64  # Simulation speed multiplier (1.0 = realtime)

    # Time step for integration
    dt::Float64

    # Composed system (rebuilt when wiring changes)
    # This is the result of composing all systems via their wiring
    composed_machine::Union{Nothing, ContinuousMachine{Float64}}
    composed_state::Vector{Float64}

    # Flag to indicate composition needs rebuild
    needs_recomposition::Bool

    # History tracking (for visualization trails)
    history_length::Int
    state_history::Dict{String, Vector{Vector{Float64}}}
end

"""
    WorldState()

Create an empty world state with default parameters.
"""
function WorldState()
    WorldState(
        Dict{String, SystemInstance}(),
        Dict{String, WireSpec}(),
        0.0,                    # time
        false,                  # running
        1.0,                    # speed
        0.001,                  # dt (1ms)
        nothing,                # composed_machine
        Float64[],              # composed_state
        false,                  # needs_recomposition
        2000,                   # history_length
        Dict{String, Vector{Vector{Float64}}}()
    )
end

# =============================================================================
# System Management
# =============================================================================

"""
    add_system!(world, id, template_id, machine, initial_state, params; position)

Add a new system instance to the world.
"""
function add_system!(
    world::WorldState,
    id::String,
    template_id::String,
    machine::ContinuousMachine{Float64},
    initial_state::Vector{Float64},
    params::Dict{String, Float64};
    position::Tuple{Float64, Float64} = (0.0, 0.0)
)
    if haskey(world.systems, id)
        error("System with id '$id' already exists")
    end

    instance = SystemInstance(
        id,
        template_id,
        machine,
        copy(initial_state),
        params,
        position
    )

    world.systems[id] = instance
    world.state_history[id] = [copy(initial_state)]
    world.needs_recomposition = true

    return instance
end

"""
    remove_system!(world, id)

Remove a system instance from the world.
Also removes any wires connected to the system.
"""
function remove_system!(world::WorldState, id::String)
    if !haskey(world.systems, id)
        error("System with id '$id' does not exist")
    end

    delete!(world.systems, id)
    delete!(world.state_history, id)

    # Remove wires connected to this system
    wires_to_remove = String[]
    for (wire_id, wire) in world.wires
        if wire.from_system == id || wire.to_system == id
            push!(wires_to_remove, wire_id)
        end
    end

    for wire_id in wires_to_remove
        delete!(world.wires, wire_id)
    end

    world.needs_recomposition = true
end

"""
    get_system(world, id)

Get a system instance by ID.
"""
function get_system(world::WorldState, id::String)
    return get(world.systems, id, nothing)
end

"""
    list_systems(world)

List all system instances in the world.
"""
function list_systems(world::WorldState)
    return collect(values(world.systems))
end

"""
    set_system_state!(world, id, state)

Set the state of a system instance.
"""
function set_system_state!(world::WorldState, id::String, state::Vector{Float64})
    sys = get_system(world, id)
    if sys === nothing
        error("System with id '$id' does not exist")
    end
    if length(state) != length(sys.state)
        error("State dimension mismatch: expected $(length(sys.state)), got $(length(state))")
    end
    sys.state .= state
end

"""
    set_system_params!(world, id, params)

Update parameters for a system instance.
Note: This may require recreating the machine with new parameters.
"""
function set_system_params!(world::WorldState, id::String, params::Dict{String, Float64})
    sys = get_system(world, id)
    if sys === nothing
        error("System with id '$id' does not exist")
    end
    merge!(sys.parameters, params)
    # Note: For parameters that affect the dynamics, we'd need to recreate the machine
    # This is handled by the server layer which has access to the system registry
end

# =============================================================================
# Wire Management
# =============================================================================

"""
    add_wire!(world, id, from_system, from_port, to_system, to_port)

Add a wire connecting two systems.
"""
function add_wire!(
    world::WorldState,
    id::String,
    from_system::String,
    from_port::Int,
    to_system::String,
    to_port::Int
)
    # Validate systems exist
    from_sys = get_system(world, from_system)
    to_sys = get_system(world, to_system)

    if from_sys === nothing
        error("Source system '$from_system' does not exist")
    end
    if to_sys === nothing
        error("Target system '$to_system' does not exist")
    end

    # Validate ports
    if from_port < 1 || from_port > noutputs(from_sys.machine)
        error("Invalid output port $from_port for system '$from_system' (has $(noutputs(from_sys.machine)) outputs)")
    end
    if to_port < 1 || to_port > ninputs(to_sys.machine)
        error("Invalid input port $to_port for system '$to_system' (has $(ninputs(to_sys.machine)) inputs)")
    end

    # Check for duplicate wire
    if haskey(world.wires, id)
        error("Wire with id '$id' already exists")
    end

    wire = WireSpec(id, from_system, from_port, to_system, to_port)
    world.wires[id] = wire
    world.needs_recomposition = true

    return wire
end

"""
    remove_wire!(world, id)

Remove a wire by ID.
"""
function remove_wire!(world::WorldState, id::String)
    if !haskey(world.wires, id)
        error("Wire with id '$id' does not exist")
    end
    delete!(world.wires, id)
    world.needs_recomposition = true
end

"""
    list_wires(world)

List all wires in the world.
"""
function list_wires(world::WorldState)
    return collect(values(world.wires))
end

"""
    get_inputs_for_system(world, system_id)

Get the input values for a system based on current wiring.
Returns a vector of input values, with 0.0 for unconnected inputs.
"""
function get_inputs_for_system(world::WorldState, system_id::String)
    sys = get_system(world, system_id)
    if sys === nothing
        return Float64[]
    end

    inputs = fill(NaN, ninputs(sys.machine))

    for wire in values(world.wires)
        if wire.to_system == system_id
            from_sys = get_system(world, wire.from_system)
            if from_sys !== nothing
                # Compute output of source system
                output = readout(from_sys.machine, from_sys.state, nothing, world.time)
                if wire.from_port <= length(output)
                    inputs[wire.to_port] = output[wire.from_port]
                end
            end
        end
    end

    return inputs
end

# =============================================================================
# World Operations
# =============================================================================

"""
    reset_world!(world)

Reset all systems to their initial states and time to 0.
Requires the system registry to recreate initial states.
"""
function reset_world!(world::WorldState, create_system_fn::Function)
    world.time = 0.0

    for sys in values(world.systems)
        _, initial_state = create_system_fn(sys.template_id, sys.parameters)
        sys.state .= initial_state
        world.state_history[sys.id] = [copy(initial_state)]
    end

    world.needs_recomposition = true
end

"""
    record_history!(world)

Record current states to history buffers.
"""
function record_history!(world::WorldState)
    for sys in values(world.systems)
        history = world.state_history[sys.id]
        push!(history, copy(sys.state))

        # Trim to max length
        while length(history) > world.history_length
            popfirst!(history)
        end
    end
end

# =============================================================================
# Serialization Helpers
# =============================================================================

"""
    serialize_system(sys)

Convert a SystemInstance to a JSON-friendly Dict.
"""
function serialize_system(sys::SystemInstance, world::WorldState)
    output = readout(sys.machine, sys.state, nothing, world.time)

    Dict(
        "id" => sys.id,
        "templateId" => sys.template_id,
        "state" => sys.state,
        "outputs" => output,
        "parameters" => sys.parameters,
        "position" => Dict("x" => sys.position[1], "y" => sys.position[2]),
        "ninputs" => ninputs(sys.machine),
        "noutputs" => noutputs(sys.machine),
        "nstates" => nstates(sys.machine)
    )
end

"""
    serialize_wire(wire, world)

Convert a WireSpec to a JSON-friendly Dict.
"""
function serialize_wire(wire::WireSpec, world::WorldState)
    # Get current value flowing through wire
    value = 0.0
    from_sys = get_system(world, wire.from_system)
    if from_sys !== nothing
        output = readout(from_sys.machine, from_sys.state, nothing, world.time)
        if wire.from_port <= length(output)
            value = output[wire.from_port]
        end
    end

    Dict(
        "id" => wire.id,
        "fromSystem" => wire.from_system,
        "fromPort" => wire.from_port,
        "toSystem" => wire.to_system,
        "toPort" => wire.to_port,
        "value" => value
    )
end

"""
    serialize_world(world)

Convert the entire world state to a JSON-friendly Dict.
"""
function serialize_world(world::WorldState)
    Dict(
        "type" => "WorldState",
        "time" => world.time,
        "running" => world.running,
        "speed" => world.speed,
        "systems" => [serialize_system(sys, world) for sys in values(world.systems)],
        "wires" => [serialize_wire(wire, world) for wire in values(world.wires)]
    )
end

"""
    serialize_state_update(world)

Create a compact state update message (just states and outputs).
"""
function serialize_state_update(world::WorldState)
    states = Dict{String, Vector{Float64}}()
    outputs = Dict{String, Vector{Float64}}()

    for sys in values(world.systems)
        states[sys.id] = sys.state
        outputs[sys.id] = readout(sys.machine, sys.state, nothing, world.time)
    end

    wire_values = Dict{String, Float64}()
    for wire in values(world.wires)
        from_sys = get_system(world, wire.from_system)
        if from_sys !== nothing
            output = readout(from_sys.machine, from_sys.state, nothing, world.time)
            if wire.from_port <= length(output)
                wire_values[wire.id] = output[wire.from_port]
            end
        end
    end

    Dict(
        "type" => "StateUpdate",
        "time" => world.time,
        "states" => states,
        "outputs" => outputs,
        "wires" => wire_values
    )
end

end # module
