module Server

using HTTP
using HTTP.WebSockets
using JSON3
using StructTypes

using AlgebraicDynamics.DWDDynam

# Import sibling modules
include("Systems.jl")
include("World.jl")
include("Composition.jl")
include("Simulation.jl")
include("CustomSystems.jl")

using .Systems
using .World
using .Composition
using .Simulation
using .CustomSystems

export start_server

# =============================================================================
# Server State
# =============================================================================

mutable struct ServerState
    world::WorldState
    composed::Union{Nothing, ComposedSystemInfo}
    clients::Vector{HTTP.WebSockets.WebSocket}
    lock::ReentrantLock
end

function ServerState()
    ServerState(
        WorldState(),
        nothing,
        HTTP.WebSockets.WebSocket[],
        ReentrantLock()
    )
end

# =============================================================================
# Recomposition
# =============================================================================

"""
    recompose!(state::ServerState)

Rebuild the composed system after wiring changes.

This is called when:
- A system is added or removed
- A wire is added or removed
- System parameters change (may require recreating machine)

The recomposition uses oapply on the current wiring diagram.
"""
function recompose!(state::ServerState)
    world = state.world

    if isempty(world.systems)
        state.composed = nothing
        world.needs_recomposition = false
        return
    end

    println("Recomposing $(length(world.systems)) systems with $(length(world.wires)) wires...")

    try
        state.composed = compose_systems(world.systems, world.wires)
        world.needs_recomposition = false
        println("Recomposition complete. Total states: $(state.composed.total_states)")
    catch e
        println("Recomposition failed: $e")
        # Fall back to independent stepping
        state.composed = nothing
        world.needs_recomposition = false
    end
end

# =============================================================================
# Message Handling
# =============================================================================

function handle_message!(state::ServerState, msg::Dict{String, Any})
    msg_type = get(msg, "type", "")
    world = state.world

    if msg_type == "AddSystem"
        return handle_add_system!(state, msg)

    elseif msg_type == "RemoveSystem"
        return handle_remove_system!(state, msg)

    elseif msg_type == "Wire"
        return handle_wire!(state, msg)

    elseif msg_type == "Unwire"
        return handle_unwire!(state, msg)

    elseif msg_type == "SetParams"
        return handle_set_params!(state, msg)

    elseif msg_type == "SetState"
        return handle_set_state!(state, msg)

    elseif msg_type == "Control"
        return handle_control!(state, msg)

    elseif msg_type == "GetTemplates"
        return Dict("type" => "Templates", "templates" => list_templates())

    elseif msg_type == "GetWorldState"
        return serialize_world(world)

    elseif msg_type == "DefineCustomSystem"
        return handle_define_custom_system!(state, msg)

    else
        return Dict("type" => "Error", "code" => "UNKNOWN_MESSAGE", "message" => "Unknown message type: $msg_type")
    end
end

function handle_add_system!(state::ServerState, msg::Dict{String, Any})
    world = state.world

    instance_id = get(msg, "instanceId", "")
    template_id = get(msg, "templateId", "")
    params_raw = get(msg, "parameters", Dict())
    position_raw = get(msg, "position", Dict("x" => 0.0, "y" => 0.0))
    initial_state_raw = get(msg, "initialState", nothing)

    if isempty(instance_id) || isempty(template_id)
        return Dict("type" => "Error", "code" => "INVALID_PARAMS", "message" => "instanceId and templateId are required")
    end

    # Convert parameters
    params = Dict{String, Float64}(String(k) => Float64(v) for (k, v) in params_raw)
    position = (Float64(get(position_raw, "x", 0.0)), Float64(get(position_raw, "y", 0.0)))

    # Create the machine
    try
        machine, default_initial = Systems.create_system(template_id, params)

        # Use provided initial state or default
        initial_state = if initial_state_raw !== nothing
            Float64.(initial_state_raw)
        else
            default_initial
        end

        add_system!(world, instance_id, template_id, machine, initial_state, params; position=position)

        println("Added system: $instance_id ($template_id)")

        # Trigger recomposition
        recompose!(state)

        return Dict("type" => "Ack", "success" => true, "action" => "AddSystem", "instanceId" => instance_id)
    catch e
        return Dict("type" => "Error", "code" => "ADD_FAILED", "message" => string(e))
    end
end

function handle_remove_system!(state::ServerState, msg::Dict{String, Any})
    world = state.world
    instance_id = get(msg, "instanceId", "")

    try
        remove_system!(world, instance_id)
        println("Removed system: $instance_id")

        # Trigger recomposition
        recompose!(state)

        return Dict("type" => "Ack", "success" => true, "action" => "RemoveSystem", "instanceId" => instance_id)
    catch e
        return Dict("type" => "Error", "code" => "REMOVE_FAILED", "message" => string(e))
    end
end

function handle_wire!(state::ServerState, msg::Dict{String, Any})
    world = state.world

    wire_id = get(msg, "wireId", "")
    from_system = get(msg, "fromSystem", "")
    from_port = get(msg, "fromPort", 0)
    to_system = get(msg, "toSystem", "")
    to_port = get(msg, "toPort", 0)

    try
        add_wire!(world, wire_id, from_system, from_port, to_system, to_port)
        println("Added wire: $wire_id ($from_system:$from_port → $to_system:$to_port)")

        # Trigger recomposition
        recompose!(state)

        return Dict("type" => "Ack", "success" => true, "action" => "Wire", "wireId" => wire_id)
    catch e
        return Dict("type" => "Error", "code" => "WIRE_FAILED", "message" => string(e))
    end
end

function handle_unwire!(state::ServerState, msg::Dict{String, Any})
    world = state.world
    wire_id = get(msg, "wireId", "")

    try
        remove_wire!(world, wire_id)
        println("Removed wire: $wire_id")

        # Trigger recomposition
        recompose!(state)

        return Dict("type" => "Ack", "success" => true, "action" => "Unwire", "wireId" => wire_id)
    catch e
        return Dict("type" => "Error", "code" => "UNWIRE_FAILED", "message" => string(e))
    end
end

function handle_set_params!(state::ServerState, msg::Dict{String, Any})
    world = state.world

    instance_id = get(msg, "instanceId", "")
    params_raw = get(msg, "parameters", Dict())
    params = Dict{String, Float64}(String(k) => Float64(v) for (k, v) in params_raw)

    try
        # For now, just update the stored params
        # Full implementation would recreate the machine with new params
        set_system_params!(world, instance_id, params)

        # TODO: Recreate machine if dynamics depend on params
        # This requires access to the system registry

        return Dict("type" => "Ack", "success" => true, "action" => "SetParams", "instanceId" => instance_id)
    catch e
        return Dict("type" => "Error", "code" => "SET_PARAMS_FAILED", "message" => string(e))
    end
end

function handle_set_state!(state::ServerState, msg::Dict{String, Any})
    world = state.world

    instance_id = get(msg, "instanceId", "")
    new_state = get(msg, "state", Float64[])

    try
        set_system_state!(world, instance_id, Float64.(new_state))
        return Dict("type" => "Ack", "success" => true, "action" => "SetState", "instanceId" => instance_id)
    catch e
        return Dict("type" => "Error", "code" => "SET_STATE_FAILED", "message" => string(e))
    end
end

function handle_control!(state::ServerState, msg::Dict{String, Any})
    world = state.world
    action = get(msg, "action", "")

    if action == "play"
        world.running = true
        println("Simulation started")

    elseif action == "pause"
        world.running = false
        println("Simulation paused")

    elseif action == "step"
        # Single step
        if state.composed !== nothing
            step_world!(world, state.composed, world.dt)
        else
            step_world_independent!(world, world.dt)
        end

    elseif action == "reset"
        world.time = 0.0
        for sys in values(world.systems)
            _, initial_state = Systems.create_system(sys.template_id, sys.parameters)
            sys.state .= initial_state
        end
        println("Simulation reset")

    elseif action == "setSpeed"
        speed = get(msg, "speed", 1.0)
        world.speed = Float64(speed)
        println("Speed set to $(world.speed)")

    elseif action == "setDt"
        dt = get(msg, "dt", 0.001)
        world.dt = Float64(dt)
        println("dt set to $(world.dt)")
    end

    return Dict("type" => "Ack", "success" => true, "action" => action)
end

function handle_define_custom_system!(state::ServerState, msg::Dict{String, Any})
    name = get(msg, "name", "")
    state_vars = String.(get(msg, "stateVars", String[]))
    equations = String.(get(msg, "equations", String[]))
    params_raw = get(msg, "parameters", [])
    inputs = String.(get(msg, "inputs", String[]))
    initial_state = Float64.(get(msg, "initialState", Float64[]))

    # Convert parameters to tuples
    params = Tuple{String, Float64}[]
    for p in params_raw
        pname = String(get(p, "name", ""))
        pdefault = Float64(get(p, "default", 1.0))
        push!(params, (pname, pdefault))
    end

    try
        success, message = validate_and_register_custom_system!(
            name, state_vars, equations, params, inputs, initial_state
        )

        if success
            # Broadcast updated template list to all clients
            broadcast_to_clients(state, Dict("type" => "Templates", "templates" => list_templates()))
            return Dict("type" => "Ack", "success" => true, "action" => "DefineCustomSystem", "message" => message)
        else
            return Dict("type" => "Error", "code" => "CUSTOM_SYSTEM_FAILED", "message" => message)
        end
    catch e
        return Dict("type" => "Error", "code" => "CUSTOM_SYSTEM_ERROR", "message" => string(e))
    end
end

# =============================================================================
# Broadcasting
# =============================================================================

function broadcast_to_clients(state::ServerState, message::Dict)
    try
        json = JSON3.write(message)
        lock(state.lock) do
            println("Broadcasting $(message["type"]) to $(length(state.clients)) clients")
            for ws in state.clients
                try
                    send(ws, json)
                catch e
                    println("Error sending to client: $e")
                end
            end
        end
    catch e
        println("Error serializing message: $e")
        println(stacktrace(catch_backtrace()))
    end
end

function broadcast_world_state(state::ServerState)
    broadcast_to_clients(state, serialize_world(state.world))
end

function broadcast_state_update(state::ServerState)
    broadcast_to_clients(state, serialize_state_update(state.world))
end

# =============================================================================
# Simulation Loop
# =============================================================================

function simulation_loop(state::ServerState)
    target_frame_time = 1/60  # 60 Hz broadcast rate

    while true
        frame_start = time()

        if state.world.running
            # Check if recomposition is needed
            if state.world.needs_recomposition
                recompose!(state)
            end

            # Calculate how many simulation steps per frame
            sim_dt = state.world.dt
            frame_dt = target_frame_time * state.world.speed
            steps_per_frame = max(1, round(Int, frame_dt / sim_dt))

            # Run simulation steps
            for _ in 1:steps_per_frame
                if state.composed !== nothing
                    step_world!(state.world, state.composed, sim_dt)
                else
                    step_world_independent!(state.world, sim_dt)
                end
            end

            # Record history for visualization trails
            record_history!(state.world)

            # Broadcast state update
            broadcast_state_update(state)
        end

        # Sleep to maintain frame rate
        elapsed = time() - frame_start
        sleep_time = target_frame_time - elapsed
        if sleep_time > 0
            sleep(sleep_time)
        end
    end
end

# =============================================================================
# WebSocket Server
# =============================================================================

function start_server(; port=8082)
    state = ServerState()

    println("===========================================")
    println("  Rectify Julia Backend")
    println("  AlgebraicDynamics + Catlab Composition")
    println("===========================================")
    println()
    println("Available system templates:")
    for template in list_templates()
        println("  - $(template["id"]): $(template["name"]) ($(template["nstates"]) states, $(template["ninputs"]) in, $(template["noutputs"]) out)")
    end
    println()
    println("Starting WebSocket server on port $port...")

    # Start simulation loop in background
    sim_task = @async simulation_loop(state)

    # Start WebSocket server
    server = WebSockets.listen!("0.0.0.0", port) do ws
        println("Client connected")

        # Add to client list
        lock(state.lock) do
            push!(state.clients, ws)
        end

        # Send initial state
        try
            # Send available templates
            send(ws, JSON3.write(Dict("type" => "Templates", "templates" => list_templates())))

            # Send current world state
            send(ws, JSON3.write(serialize_world(state.world)))
        catch e
            println("Error sending initial state: $e")
        end

        # Message handling loop
        try
            for msg_str in ws
                try
                    msg = JSON3.read(msg_str, Dict{String, Any})
                    response = handle_message!(state, msg)

                    # Send response
                    send(ws, JSON3.write(response))

                    # If it was a mutation, broadcast new world state
                    msg_type = get(msg, "type", "")
                    if msg_type in ["AddSystem", "RemoveSystem", "Wire", "Unwire", "SetState", "SetParams", "Control"]
                        broadcast_world_state(state)
                    end
                catch e
                    println("Error handling message: $e")
                    send(ws, JSON3.write(Dict("type" => "Error", "message" => string(e))))
                end
            end
        catch e
            if !(e isa HTTP.WebSockets.WebSocketError)
                println("WebSocket error: $e")
            end
        end

        # Remove from client list
        lock(state.lock) do
            filter!(c -> c !== ws, state.clients)
        end

        println("Client disconnected")
    end

    println("Server running. Press Ctrl+C to stop.")
    println()

    try
        wait(server)
    catch e
        if e isa InterruptException
            println("\nShutting down...")
            close(server)
        else
            rethrow(e)
        end
    end
end

end # module
