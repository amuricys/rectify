module Composition

using AlgebraicDynamics.DWDDynam
using Catlab.WiringDiagrams
using Catlab.Programs

export compose_systems, ComposedSystemInfo

# =============================================================================
# Composition via Catlab Directed Wiring Diagrams
# =============================================================================

"""
    ComposedSystemInfo

Result of composing systems via a wiring diagram.
Contains the composed machine and mappings to track which parts of the
composed state correspond to which original systems.
"""
struct ComposedSystemInfo
    # The composed machine from oapply
    machine::ContinuousMachine{Float64}

    # Mapping from system_id to its state indices in the composed state
    # system_ranges[sys_id] = start_idx:end_idx
    system_ranges::Dict{String, UnitRange{Int}}

    # Ordered list of system IDs (matches box order in diagram)
    system_order::Vector{String}

    # Total number of states in composed system
    total_states::Int
end

"""
    compose_systems(systems, wires)

Compose open dynamical systems according to wiring specification.

For directed wiring diagrams (DWD), we use oapply to compose machines.
Each wire connects an output port of one system to an input port of another.

Arguments:
- systems: Dict{String, SystemInstance} - systems with .machine and .id fields
- wires: Dict{String, WireSpec} - wires with from_system, from_port, to_system, to_port

Returns:
- ComposedSystemInfo or nothing if no systems
"""
function compose_systems(
    systems::Dict{String, T},
    wires::Dict{String, W}
) where {T, W}

    if isempty(systems)
        return nothing
    end

    # Deterministic ordering of systems
    system_order = sort(collect(keys(systems)))
    n_systems = length(system_order)

    # Build index lookup: system_id -> box index (1-based)
    sys_to_box = Dict(id => i for (i, id) in enumerate(system_order))

    # Collect machines in box order
    machines = [systems[id].machine for id in system_order]

    # If only one system with no wires, return it directly
    if n_systems == 1 && isempty(wires)
        sys = systems[system_order[1]]
        system_ranges = Dict(system_order[1] => 1:nstates(sys.machine))
        return ComposedSystemInfo(
            sys.machine,
            system_ranges,
            system_order,
            nstates(sys.machine)
        )
    end

    # Build a WiringDiagram for composition
    # For DWD, we need to specify the outer box type and inner boxes
    #
    # The oapply function takes a WiringDiagram and a list of machines
    # and returns a composed machine.
    #
    # Strategy for multiple systems without full wiring:
    # - Create a diagram with all systems as boxes
    # - Wire them according to the wire specifications
    # - Expose unconnected inputs as outer inputs
    # - Expose all outputs as outer outputs

    # For simplicity, we'll manually compose by running systems in parallel
    # and routing signals according to wires. This avoids complex diagram construction
    # while still being correct.

    # Build wire lookup: for each (to_system, to_port), what is (from_system, from_port)?
    wire_map = Dict{Tuple{String, Int}, Tuple{String, Int}}()
    for wire in values(wires)
        wire_map[(wire.to_system, wire.to_port)] = (wire.from_system, wire.from_port)
    end

    # Calculate state ranges
    system_ranges = Dict{String, UnitRange{Int}}()
    offset = 0
    for sys_id in system_order
        m = systems[sys_id].machine
        n = nstates(m)
        system_ranges[sys_id] = (offset + 1):(offset + n)
        offset += n
    end
    total_states = offset

    # Composed dynamics: run all systems, gather inputs from wires
    function composed_dynamics(u, external_inputs, p, t)
        du = zeros(total_states)

        # First compute all outputs (needed for wiring)
        outputs = Dict{String, Vector{Float64}}()
        for sys_id in system_order
            haskey(systems, sys_id) || continue
            sys = systems[sys_id]
            range = system_ranges[sys_id]
            sys_state = u[range]
            outputs[sys_id] = readout(sys.machine, sys_state, p, t)
        end

        # Now compute dynamics for each system
        for sys_id in system_order
            haskey(systems, sys_id) || continue
            sys = systems[sys_id]
            range = system_ranges[sys_id]
            sys_state = u[range]

            # Build inputs for this system from wires
            # Use NaN for unconnected inputs (expanded input convention)
            n_in = ninputs(sys.machine)
            inputs = fill(NaN, n_in)
            for port in 1:n_in
                if haskey(wire_map, (sys_id, port))
                    from_id, from_port = wire_map[(sys_id, port)]
                    if haskey(outputs, from_id) && from_port <= length(outputs[from_id])
                        inputs[port] = outputs[from_id][from_port]
                    end
                end
            end

            # Evaluate dynamics
            sys_du = eval_dynamics(sys.machine, sys_state, inputs, p, t)
            du[range] .= sys_du
        end

        return du
    end

    # Composed readout: concatenate all outputs
    function composed_readout(u, p, t)
        outputs = Float64[]
        for sys_id in system_order
            sys = systems[sys_id]
            range = system_ranges[sys_id]
            sys_state = u[range]
            append!(outputs, readout(sys.machine, sys_state, p, t))
        end
        return outputs
    end

    # Total external inputs (0 for now - all inputs come from wires)
    total_external_inputs = 0
    total_outputs = sum(noutputs(m) for m in machines)

    composed_machine = ContinuousMachine{Float64}(
        total_external_inputs,
        total_states,
        total_outputs,
        composed_dynamics,
        composed_readout
    )

    return ComposedSystemInfo(
        composed_machine,
        system_ranges,
        system_order,
        total_states
    )
end

"""
    extract_composed_state(info::ComposedSystemInfo, systems)

Build the composed state vector from individual system states.
"""
function extract_composed_state(
    info::ComposedSystemInfo,
    systems::Dict{String, T}
) where T
    state = zeros(info.total_states)
    for sys_id in info.system_order
        if haskey(systems, sys_id)
            range = info.system_ranges[sys_id]
            state[range] .= systems[sys_id].state
        end
    end
    return state
end

"""
    distribute_composed_state!(info::ComposedSystemInfo, composed_state, systems)

Distribute the composed state back to individual system states.
"""
function distribute_composed_state!(
    info::ComposedSystemInfo,
    composed_state::Vector{Float64},
    systems::Dict{String, T}
) where T
    for sys_id in info.system_order
        if haskey(systems, sys_id)
            range = info.system_ranges[sys_id]
            systems[sys_id].state .= composed_state[range]
        end
    end
end

end # module
