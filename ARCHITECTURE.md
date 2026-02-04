# Rectify: Architecture for Open Dynamical Systems Visualizer

## 1. System Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              FRONTEND (Svelte 5)                            │
│                                                                             │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────────┐  │
│  │  Tab: Optimize  │  │  Tab: Dynamics  │  │      Global Controls        │  │
│  │  (TSP, GA, etc) │  │  (Open Systems) │  │  Play/Pause/Reset/Speed     │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────────────────┘  │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                         Three.js Canvas                                 ││
│  │  ┌───────────┐    ┌───────────┐    ┌───────────┐                       ││
│  │  │  Box A    │───▶│  Box B    │───▶│  Box C    │                       ││
│  │  │ (Lorenz)  │    │ (Scalar)  │    │ (Custom)  │                       ││
│  │  │  x,y,z    │    │    k      │    │  a,b      │                       ││
│  │  └───────────┘    └───────────┘    └───────────┘                       ││
│  │       │                                  ▲                              ││
│  │       └──────────────────────────────────┘                              ││
│  │                   (wiring connections)                                  ││
│  └─────────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ WebSocket (JSON)
                                    │ Port 8082
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           BACKEND (Julia)                                   │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                        Session Manager                                  ││
│  │  - WebSocket connections                                                ││
│  │  - Command dispatch                                                     ││
│  │  - State broadcasting                                                   ││
│  └─────────────────────────────────────────────────────────────────────────┘│
│                                    │                                        │
│                                    ▼                                        │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                         World State                                     ││
│  │  - Registry of open systems                                             ││
│  │  - Wiring diagram (Catlab UWD)                                          ││
│  │  - Current composed system                                              ││
│  │  - Solver instance                                                      ││
│  └─────────────────────────────────────────────────────────────────────────┘│
│                                    │                                        │
│                                    ▼                                        │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                    Incremental Simulation Loop                          ││
│  │  - Rolling time window                                                  ││
│  │  - Euler / RK4 stepper                                                  ││
│  │  - State emission at ~60 Hz                                             ││
│  └─────────────────────────────────────────────────────────────────────────┘│
│                                                                             │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐          │
│  │  AlgebraicDynamics│  │     Catlab       │  │ DiffEq.jl        │          │
│  │  ContinuousMachine│  │  UndirectedWiring│  │ (future: stiff)  │          │
│  └──────────────────┘  └──────────────────┘  └──────────────────┘          │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Core Abstractions

### 2.1 Open Dynamical System

An **open system** is a dynamical system with explicit inputs and outputs:

```
         inputs                    outputs
           │                          │
           ▼                          │
    ┌──────────────┐                  │
    │              │                  │
    │   dx/dt = f(x, u)               │
    │   y = g(x)   │──────────────────┘
    │              │
    └──────────────┘
         state x
```

In AlgebraicDynamics terms, this is a `ContinuousMachine{T}` with:
- `ninputs::Int` — number of input ports
- `nstates::Int` — number of state variables
- `noutputs::Int` — number of output ports
- `dynamics(u, x, p, t)` — vector field (returns dx/dt)
- `readout(x, p)` — output function

### 2.2 Wiring Diagram

A **wiring diagram** specifies how open systems compose. We use Catlab's
`UndirectedWiringDiagram` to represent:

- **Boxes**: each box is an open system instance
- **Junctions**: shared variables that connect outputs to inputs
- **Outer ports**: exposed I/O for the composed system

When wiring changes, we must:
1. Rebuild the composed `ContinuousMachine`
2. Reinitialize the solver with the new system
3. Preserve state where possible (or reset)

### 2.3 System Registry

The backend maintains a **registry** of available system templates:

| ID         | Name              | States | Inputs | Outputs | Parameters        |
|------------|-------------------|--------|--------|---------|-------------------|
| lorenz     | Lorenz Attractor  | 3      | 1      | 3       | σ, ρ, β           |
| vanderpol  | Van der Pol       | 2      | 1      | 2       | μ                 |
| harmonic   | Harmonic Osc.     | 2      | 1      | 2       | ω, ζ              |
| duffing    | Duffing Osc.      | 2      | 1      | 2       | α, β, δ, γ, ω     |
| constant   | Constant Source   | 0      | 0      | 1       | value             |
| scaler     | Signal Scaler     | 0      | 1      | 1       | k                 |
| custom     | User-Defined      | N      | M      | P       | user-specified    |

---

## 3. Protocol Specification

### 3.1 Frontend → Backend Messages

```typescript
// Add a new system instance to the world
type AddSystem = {
  type: "AddSystem";
  templateId: string;        // e.g., "lorenz"
  instanceId: string;        // unique ID for this instance
  initialState?: number[];   // optional initial conditions
  parameters?: Record<string, number>;
  position?: { x: number; y: number };  // UI position (for state sync)
};

// Remove a system instance
type RemoveSystem = {
  type: "RemoveSystem";
  instanceId: string;
};

// Create a wire between systems
type Wire = {
  type: "Wire";
  wireId: string;
  fromSystem: string;
  fromPort: number;          // output port index (0-based)
  toSystem: string;
  toPort: number;            // input port index (0-based)
};

// Remove a wire
type Unwire = {
  type: "Unwire";
  wireId: string;
};

// Update system parameters
type SetParams = {
  type: "SetParams";
  instanceId: string;
  parameters: Record<string, number>;
};

// Update system state directly
type SetState = {
  type: "SetState";
  instanceId: string;
  state: number[];
};

// Control simulation
type Control = {
  type: "Control";
  action: "play" | "pause" | "step" | "reset";
  dt?: number;               // time step override
  speed?: number;            // simulation speed multiplier
};

// Define a custom system
type DefineSystem = {
  type: "DefineSystem";
  templateId: string;
  name: string;
  nstates: number;
  ninputs: number;
  noutputs: number;
  parameters: string[];      // parameter names
  dynamics: string;          // Julia expression for dx/dt
  readout: string;           // Julia expression for outputs
};
```

### 3.2 Backend → Frontend Messages

```typescript
// Full state update (sent on connection and after rewiring)
type WorldState = {
  type: "WorldState";
  time: number;
  running: boolean;
  systems: SystemState[];
  wires: WireState[];
};

type SystemState = {
  instanceId: string;
  templateId: string;
  state: number[];           // current state vector
  outputs: number[];         // current output values
  parameters: Record<string, number>;
  position: { x: number; y: number };
};

type WireState = {
  wireId: string;
  fromSystem: string;
  fromPort: number;
  toSystem: string;
  toPort: number;
  value: number;             // current signal value
};

// Incremental update (sent every frame during simulation)
type StateUpdate = {
  type: "StateUpdate";
  time: number;
  states: Record<string, number[]>;   // instanceId → state
  outputs: Record<string, number[]>;  // instanceId → outputs
  wires: Record<string, number>;      // wireId → value
};

// Error notification
type Error = {
  type: "Error";
  code: string;
  message: string;
  details?: any;
};

// Acknowledgment (for commands that need confirmation)
type Ack = {
  type: "Ack";
  requestId?: string;
  success: boolean;
};
```

---

## 4. Rewiring and Solver Reconstruction

### 4.1 The Problem

When the user modifies the wiring diagram:
1. The composed system structure changes
2. The ODE solver must be reconstructed
3. Julia may need to JIT-compile new code paths

This creates latency that must be managed.

### 4.2 Strategy

```
User Rewires ──▶ Pause Simulation
                      │
                      ▼
              Build New UWD
                      │
                      ▼
              Compose Systems (AlgebraicDynamics.oapply)
                      │
                      ▼
              Extract State from Old Solver
                      │
                      ▼
              Create New Stepper Instance
                      │
                      ▼
              Resume Simulation
```

**Key decisions:**
- **Pause automatically** when rewiring to avoid inconsistent states
- **Preserve state** where possible (systems that weren't removed)
- **Reset state** for newly added systems or when topology changes significantly
- **Batch rewiring**: if multiple changes come in rapid succession, debounce
  and apply them together

### 4.3 Performance Considerations

| Operation                  | Expected Latency | Mitigation                    |
|----------------------------|------------------|-------------------------------|
| Add system                 | 10-50ms          | Pre-compile common templates  |
| Remove system              | 5-20ms           | —                             |
| Add wire                   | 50-200ms         | Composition is O(n²) worst    |
| Full recomposition         | 100-500ms        | Cache partial compositions    |
| JIT compilation (first)    | 500ms-2s         | Warm up on startup            |

**Mitigation strategies:**
1. **Precompilation**: Common system templates are precompiled on startup
2. **Warm-up**: Run a dummy simulation on startup to trigger JIT
3. **Incremental composition**: When adding a single wire, don't recompute everything
4. **Background recomposition**: Start building new system while old continues running

---

## 5. Real-Time Simulation Architecture

### 5.1 Simulation Loop

```julia
function simulation_loop(world::WorldState)
    target_dt = 1/60  # 60 Hz target frame rate
    sim_dt = 0.001    # internal simulation timestep

    while world.running
        frame_start = time()

        # Advance simulation time
        steps_per_frame = round(Int, world.speed * target_dt / sim_dt)
        for _ in 1:steps_per_frame
            step!(world, sim_dt)
        end

        # Broadcast state
        broadcast_state(world)

        # Sleep to maintain frame rate
        elapsed = time() - frame_start
        sleep_time = target_dt - elapsed
        if sleep_time > 0
            sleep(sleep_time)
        end
    end
end

function step!(world::WorldState, dt::Float64)
    # 1. Gather inputs from wires
    inputs = gather_inputs(world)

    # 2. Evaluate dynamics of composed system
    # (or step each system individually if not composed)
    if world.composed_system !== nothing
        x = get_composed_state(world)
        dx = world.composed_system.dynamics(inputs, x, world.params, world.time)
        x_new = x .+ dt .* dx
        set_composed_state!(world, x_new)
    else
        for sys in world.systems
            step_system!(sys, inputs[sys.id], dt, world.time)
        end
    end

    # 3. Compute outputs
    for sys in world.systems
        sys.outputs = compute_outputs(sys)
    end

    # 4. Advance time
    world.time += dt
end
```

### 5.2 State Management

Each system instance maintains:
```julia
mutable struct SystemInstance
    id::String
    template::SystemTemplate
    state::Vector{Float64}
    parameters::Dict{Symbol, Float64}
    outputs::Vector{Float64}
    position::Tuple{Float64, Float64}  # UI position
end
```

The world state:
```julia
mutable struct WorldState
    time::Float64
    running::Bool
    speed::Float64

    # System instances
    systems::Dict{String, SystemInstance}

    # Wiring
    wires::Dict{String, WireSpec}

    # Composed system (rebuilt on rewire)
    composed_machine::Union{Nothing, ContinuousMachine}
    composed_state::Vector{Float64}

    # Wiring diagram (Catlab)
    wiring_diagram::Union{Nothing, UndirectedWiringDiagram}
end
```

---

## 6. Frontend Architecture

### 6.1 Component Hierarchy

```
App.svelte
├── TabBar.svelte
│   ├── OptimizationTab (existing)
│   └── DynamicsTab (new)
│
├── GlobalControls.svelte
│   ├── PlayPauseButton
│   ├── ResetButton
│   └── SpeedSlider
│
└── MainCanvas.svelte
    ├── [if optimization tab]
    │   └── TSPRenderer.svelte (existing)
    │
    └── [if dynamics tab]
        └── DynamicsCanvas.svelte
            ├── SystemBox.svelte (repeated)
            │   ├── SystemHeader
            │   ├── TrajectoryView (Three.js)
            │   ├── PortList
            │   └── ParameterControls
            │
            └── WireOverlay.svelte
                └── Wire.svelte (repeated)
```

### 6.2 State Management

```typescript
// dynamics.svelte.ts - Svelte 5 runes-based store

class DynamicsConnection {
  ws: WebSocket | null = $state(null);
  connected = $state(false);

  // World state (reactive)
  time = $state(0);
  running = $state(false);
  speed = $state(1);
  systems = $state<Map<string, SystemState>>(new Map());
  wires = $state<Map<string, WireState>>(new Map());

  // Derived
  systemList = $derived([...this.systems.values()]);
  wireList = $derived([...this.wires.values()]);

  // Commands
  addSystem(templateId: string, position: {x: number, y: number}) { ... }
  removeSystem(instanceId: string) { ... }
  wire(from: PortRef, to: PortRef) { ... }
  unwire(wireId: string) { ... }
  setParams(instanceId: string, params: Record<string, number>) { ... }
  play() { ... }
  pause() { ... }
  reset() { ... }
}
```

### 6.3 Three.js Integration

Each SystemBox contains an embedded Three.js scene showing:
- The system's trajectory in state space (2D or 3D projection)
- Real-time updates as state evolves
- Visual indication of inputs/outputs

```typescript
// TrajectoryView.svelte
// Uses a rolling buffer for efficient rendering

const MAX_POINTS = 2000;
const positions = new Float32Array(MAX_POINTS * 3);
let head = 0;

function addPoint(x: number, y: number, z: number) {
  const i = head * 3;
  positions[i] = x;
  positions[i + 1] = y;
  positions[i + 2] = z;
  head = (head + 1) % MAX_POINTS;

  // Update geometry
  geometry.attributes.position.needsUpdate = true;
}
```

---

## 7. Extensibility

### 7.1 Adding New System Templates

1. Define the template in Julia:
```julia
function rossler_machine(; a=0.2, b=0.2, c=5.7)
    ContinuousMachine{Float64}(
        1, 3, 3,  # ninputs, nstates, noutputs
        # dynamics: (u, x, p, t) -> dx/dt
        (u, x, p, t) -> [
            -x[2] - x[3],
            x[1] + a*x[2] + u[1],
            b + x[3]*(x[1] - c)
        ],
        # readout: (x, p) -> y
        (x, p) -> x
    )
end
```

2. Register in the system registry
3. Frontend automatically discovers via registry query

### 7.2 User-Defined Systems

Users can define systems via the UI:
1. Specify dimensions (states, inputs, outputs)
2. Enter dynamics equations (parsed to Julia expressions)
3. System is compiled and added to registry

**Safety**: User-defined dynamics run in a sandboxed evaluator with:
- No file system access
- No network access
- Timeout on evaluation
- Restricted to mathematical operations

### 7.3 Future Extensions

| Extension              | Difficulty | Notes                                    |
|------------------------|------------|------------------------------------------|
| Discrete-time systems  | Medium     | Use `DiscreteMachine` from AlgebraicDynamics |
| Hybrid systems         | Hard       | Need callback-based ODE solver           |
| Stochastic systems     | Medium     | Add noise term to dynamics               |
| FIR/IIR filters        | Easy       | Special case of discrete systems         |
| PDE systems            | Very Hard  | Would need spatial discretization        |
| Delay systems          | Hard       | Need history-aware solver                |

---

## 8. Performance Targets

| Metric                       | Target    | Notes                              |
|------------------------------|-----------|------------------------------------|
| Frame rate                   | 60 Hz     | WebSocket broadcast rate           |
| Latency (state → display)    | < 50ms    | Network + render pipeline          |
| Rewiring latency             | < 500ms   | Full recomposition                 |
| Startup time                 | < 3s      | Including JIT warm-up              |
| Systems supported            | 10+       | Before performance degradation     |
| State dimensions             | 100+      | Total across all systems           |

---

## 9. File Structure

```
rectify/
├── rectify-frontend/
│   ├── src/
│   │   ├── routes/
│   │   │   └── +page.svelte
│   │   └── lib/
│   │       ├── components/
│   │       │   ├── dynamics/
│   │       │   │   ├── DynamicsCanvas.svelte
│   │       │   │   ├── SystemBox.svelte
│   │       │   │   ├── TrajectoryView.svelte
│   │       │   │   ├── WireOverlay.svelte
│   │       │   │   └── Wire.svelte
│   │       │   ├── optimization/
│   │       │   │   ├── TSPRenderer.svelte
│   │       │   │   └── OptimizationControls.svelte
│   │       │   ├── GlobalControls.svelte
│   │       │   └── TabBar.svelte
│   │       └── stores/
│   │           ├── dynamics.svelte.ts
│   │           └── optimization.svelte.ts
│   └── package.json
│
├── rectify-julia/
│   ├── src/
│   │   ├── Rectify.jl           # Main module
│   │   ├── Server.jl            # WebSocket server
│   │   ├── Systems.jl           # System templates
│   │   ├── World.jl             # World state management
│   │   ├── Composition.jl       # Wiring diagram composition
│   │   └── Simulation.jl        # Incremental simulation loop
│   ├── Project.toml
│   └── run.jl
│
└── ARCHITECTURE.md              # This document
```

---

## 10. Summary

This architecture provides:

1. **Formal compositional semantics** via AlgebraicDynamics/Catlab
2. **Real-time visualization** with 60 Hz updates
3. **Interactive rewiring** with managed latency
4. **Extensibility** for new system types
5. **Clean separation** between frontend visualization and backend computation

The key insight is that **rewiring is expensive** (requires recomposition and potentially JIT),
so we must:
- Pause during rewiring
- Preserve state where possible
- Pre-warm common code paths
- Give feedback to users about compilation status

This is a research-grade tool, not a toy demo.
