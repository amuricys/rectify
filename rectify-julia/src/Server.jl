module Server

using HTTP
using HTTP.WebSockets
using JSON3
using StructTypes

using AlgebraicDynamics.DWDDynam
using Catlab.WiringDiagrams
using DifferentialEquations

include("Systems.jl")
using .Systems

export start_server

# ============================================================================
# Protocol types
# ============================================================================

abstract type Message end

struct AddSystem <: Message
    id::String
    kind::String
    params::Dict{String, Float64}
end

struct RemoveSystem <: Message
    id::String
end

struct Wire <: Message
    from_system::String
    from_port::Int
    to_system::String
    to_port::Int
end

struct Unwire <: Message
    from_system::String
    from_port::Int
    to_system::String
    to_port::Int
end

struct SetState <: Message
    system_id::String
    state::Vector{Float64}
end

struct Control <: Message
    action::String  # "play", "pause", "step", "reset"
end

# JSON parsing
StructTypes.StructType(::Type{AddSystem}) = StructTypes.Struct()
StructTypes.StructType(::Type{RemoveSystem}) = StructTypes.Struct()
StructTypes.StructType(::Type{Wire}) = StructTypes.Struct()
StructTypes.StructType(::Type{Control}) = StructTypes.Struct()

# ============================================================================
# World state
# ============================================================================

mutable struct SystemInstance
    id::String
    kind::String
    machine::ContinuousMachine{Float64}
    state::Vector{Float64}
    params::Dict{String, Float64}
end

mutable struct WorldState
    systems::Dict{String, SystemInstance}
    wires::Vector{Tuple{String, Int, String, Int}}  # (from_id, from_port, to_id, to_port)
    running::Bool
    t::Float64
    dt::Float64
end

function WorldState()
    WorldState(
        Dict{String, SystemInstance}(),
        Vector{Tuple{String, Int, String, Int}}(),
        false,
        0.0,
        0.01
    )
end

# ============================================================================
# System factory
# ============================================================================

function create_system(kind::String, params::Dict{String, Float64})
    if kind == "lorenz"
        σ = get(params, "sigma", 10.0)
        ρ = get(params, "rho", 28.0)
        β = get(params, "beta", 8/3)
        machine = lorenz_machine(; σ=σ, ρ=ρ, β=β)
        initial_state = [1.0, 1.0, 1.0]
        return machine, initial_state
    elseif kind == "harmonic"
        m = get(params, "m", 1.0)
        k = get(params, "k", 1.0)
        damping = get(params, "damping", 0.0)
        machine = harmonic_machine(; m=m, k=k, damping=damping)
        initial_state = [1.0, 0.0]
        return machine, initial_state
    elseif kind == "vanderpol"
        μ = get(params, "mu", 1.0)
        machine = vanderpol_machine(; μ=μ)
        initial_state = [1.0, 0.0]
        return machine, initial_state
    elseif kind == "duffing"
        δ = get(params, "delta", 0.3)
        α = get(params, "alpha", -1.0)
        β = get(params, "beta", 1.0)
        γ = get(params, "gamma", 0.5)
        ω = get(params, "omega", 1.2)
        machine = duffing_machine(; δ=δ, α=α, β=β, γ=γ, ω=ω)
        initial_state = [1.0, 0.0]
        return machine, initial_state
    else
        error("Unknown system kind: $kind")
    end
end

# ============================================================================
# Simulation step
# ============================================================================

function step_world!(world::WorldState)
    # For now, step each system independently
    # TODO: Use AlgebraicDynamics composition for wired systems

    for (id, sys) in world.systems
        # Gather inputs from wires
        inputs = zeros(ninputs(sys.machine))

        for (from_id, from_port, to_id, to_port) in world.wires
            if to_id == id && to_port <= length(inputs)
                from_sys = get(world.systems, from_id, nothing)
                if from_sys !== nothing
                    # Get output from source system
                    output = readout(from_sys.machine, from_sys.state, nothing, world.t)
                    if from_port <= length(output)
                        inputs[to_port] = output[from_port]
                    end
                end
            end
        end

        # Simple Euler step (for demonstration; could use DifferentialEquations.jl)
        du = eval_dynamics(sys.machine, sys.state, inputs, nothing, world.t)
        sys.state .+= world.dt .* du
    end

    world.t += world.dt
end

# ============================================================================
# Message handling
# ============================================================================

function handle_message!(world::WorldState, msg::Dict)
    msg_type = get(msg, "type", "")

    if msg_type == "add_system"
        id = msg["id"]
        kind = msg["kind"]
        params = Dict{String, Float64}(
            String(k) => Float64(v) for (k, v) in get(msg, "params", Dict())
        )
        machine, initial_state = create_system(kind, params)
        world.systems[id] = SystemInstance(id, kind, machine, initial_state, params)
        println("Added system: $id ($kind)")
        return Dict("type" => "system_added", "id" => id)

    elseif msg_type == "remove_system"
        id = msg["id"]
        delete!(world.systems, id)
        # Remove associated wires
        filter!(w -> w[1] != id && w[3] != id, world.wires)
        println("Removed system: $id")
        return Dict("type" => "system_removed", "id" => id)

    elseif msg_type == "wire"
        from_id = msg["from_system"]
        from_port = msg["from_port"]
        to_id = msg["to_system"]
        to_port = msg["to_port"]
        push!(world.wires, (from_id, from_port, to_id, to_port))
        println("Wired: $from_id:$from_port → $to_id:$to_port")
        return Dict("type" => "wired", "from" => from_id, "to" => to_id)

    elseif msg_type == "unwire"
        from_id = msg["from_system"]
        from_port = msg["from_port"]
        to_id = msg["to_system"]
        to_port = msg["to_port"]
        filter!(w -> w != (from_id, from_port, to_id, to_port), world.wires)
        return Dict("type" => "unwired")

    elseif msg_type == "control"
        action = msg["action"]
        if action == "play"
            world.running = true
        elseif action == "pause"
            world.running = false
        elseif action == "step"
            step_world!(world)
        elseif action == "reset"
            world.t = 0.0
            for (id, sys) in world.systems
                _, initial = create_system(sys.kind, sys.params)
                sys.state .= initial
            end
        end
        return Dict("type" => "control_ack", "action" => action)

    elseif msg_type == "get_state"
        return build_state_message(world)

    else
        return Dict("type" => "error", "message" => "Unknown message type: $msg_type")
    end
end

function build_state_message(world::WorldState)
    systems_state = Dict{String, Any}()

    for (id, sys) in world.systems
        output = readout(sys.machine, sys.state, nothing, world.t)
        systems_state[id] = Dict(
            "kind" => sys.kind,
            "state" => sys.state,
            "output" => output,
            "ninputs" => ninputs(sys.machine),
            "noutputs" => noutputs(sys.machine)
        )
    end

    Dict(
        "type" => "state",
        "t" => world.t,
        "running" => world.running,
        "systems" => systems_state,
        "wires" => [
            Dict("from_system" => w[1], "from_port" => w[2],
                 "to_system" => w[3], "to_port" => w[4])
            for w in world.wires
        ]
    )
end

# ============================================================================
# WebSocket server
# ============================================================================

function start_server(; port=8082)
    world = WorldState()

    # Add a default Lorenz system for testing
    machine, initial_state = create_system("lorenz", Dict{String, Float64}())
    world.systems["lorenz1"] = SystemInstance("lorenz1", "lorenz", machine, initial_state, Dict{String, Float64}())

    println("Starting AlgebraicDynamics server on port $port...")

    server = WebSockets.listen!("0.0.0.0", port) do ws
        println("Client connected")

        # Send initial state
        send(ws, JSON3.write(build_state_message(world)))

        # Simulation loop in background task
        sim_task = @async begin
            while isopen(ws)
                if world.running
                    step_world!(world)
                    try
                        send(ws, JSON3.write(build_state_message(world)))
                    catch e
                        break
                    end
                end
                sleep(0.016)  # ~60fps
            end
        end

        # Message handling loop
        try
            for msg in ws
                parsed = JSON3.read(msg, Dict{String, Any})
                response = handle_message!(world, parsed)
                send(ws, JSON3.write(response))

                # Also send full state after any mutation
                send(ws, JSON3.write(build_state_message(world)))
            end
        catch e
            if !(e isa HTTP.WebSockets.WebSocketError)
                println("Error: $e")
            end
        end

        println("Client disconnected")
    end

    println("Server running. Press Ctrl+C to stop.")

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
