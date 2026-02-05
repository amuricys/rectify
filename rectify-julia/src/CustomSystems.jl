module CustomSystems

using AlgebraicDynamics.DWDDynam

# Import Systems module for registry access
using ..Systems: SystemTemplate, SYSTEM_REGISTRY

export validate_and_register_custom_system!

# =============================================================================
# Expression Validation
# =============================================================================

"""
    ALLOWED_FUNCTIONS

Whitelist of math functions allowed in custom expressions.
"""
const ALLOWED_FUNCTIONS = Set([
    "sin", "cos", "tan", "exp", "log", "sqrt", "abs", "tanh",
    "min", "max", "pi"
])

"""
    FORBIDDEN_PATTERNS

Patterns that are explicitly blocked for safety.
"""
const FORBIDDEN_PATTERNS = [
    r"eval\s*\(",
    r"ccall\s*\(",
    r"@\w+",           # macros
    r"\.\w+",          # dot access (field access / module access)
    r"\"",             # string literals
    r"'",              # char literals
    r"import\b",
    r"using\b",
    r"include\b",
    r"require\b",
    r"run\s*\(",
    r"open\s*\(",
    r"read\s*\(",
    r"write\s*\(",
    r"ENV\b",
    r"Base\b",
    r"Core\b",
    r"Main\b",
    r"Module\b",
    r"Symbol\b",
    r"Expr\b",
    r"Meta\b",
    r"unsafe_",
    r"pointer",
    r"Ptr\b",
    r"Ref\b",
]

"""
    validate_expression(expr::String, state_vars::Vector{String}, param_names::Vector{String})

Validate that an expression is safe to compile.
Returns nothing on success, or an error message string.
"""
function validate_expression(
    expr::String,
    state_vars::Vector{String},
    param_names::Vector{String},
    input_names::Vector{String}
)
    # Check forbidden patterns
    for pattern in FORBIDDEN_PATTERNS
        if occursin(pattern, expr)
            return "Forbidden construct in expression: $(expr)"
        end
    end

    # Tokenize and check identifiers
    tokens = collect(eachmatch(r"[a-zA-Z_][a-zA-Z0-9_]*", expr))
    allowed_ids = Set(vcat(state_vars, param_names, input_names, collect(ALLOWED_FUNCTIONS), ["t", "pi"]))

    for m in tokens
        tok = m.match
        if !(tok in allowed_ids)
            return "Unknown identifier '$tok' in expression. Declare it as a parameter or check spelling."
        end
    end

    # Check for balanced parentheses
    depth = 0
    for ch in expr
        if ch == '('
            depth += 1
        elseif ch == ')'
            depth -= 1
        end
        if depth < 0
            return "Unbalanced parentheses in expression"
        end
    end
    if depth != 0
        return "Unbalanced parentheses in expression"
    end

    return nothing
end

# =============================================================================
# Custom System Builder
# =============================================================================

"""
    build_custom_dynamics(state_vars, equations, param_names, param_defaults, input_names)

Build a dynamics function from expression strings.
Uses Meta.parse + eval in a restricted sandbox.

The generated machine follows the expanded-input convention:
  - Ports 1..nstates: state replacement inputs (NaN = not connected)
  - Ports nstates+1..nstates+nparams: parameter inputs
"""
function build_custom_dynamics(
    state_vars::Vector{String},
    equations::Vector{String},
    param_names::Vector{String},
    param_defaults::Vector{Float64},
    input_names::Vector{String}
)
    nstates = length(state_vars)
    nparams = length(param_names)
    nextra_inputs = length(input_names)
    total_inputs = nstates + nparams + nextra_inputs

    # Build the dynamics function as a string, then compile it
    # This avoids issues with closures over parsed expressions

    # Build variable binding code
    state_bindings = String[]
    for (i, var) in enumerate(state_vars)
        push!(state_bindings, "$(var) = u[$i]")
    end

    # State replacement inputs
    state_eff_bindings = String[]
    for (i, var) in enumerate(state_vars)
        push!(state_eff_bindings,
            "$(var)_eff = (length(x) >= $i && !isnan(x[$i])) ? x[$i] : $var")
    end

    # Parameter inputs
    param_bindings = String[]
    for (j, pname) in enumerate(param_names)
        port_idx = nstates + j
        push!(param_bindings,
            "$(pname) = (length(x) >= $port_idx && !isnan(x[$port_idx])) ? x[$port_idx] : $(param_defaults[j])")
    end

    # Extra input bindings
    extra_bindings = String[]
    for (k, iname) in enumerate(input_names)
        port_idx = nstates + nparams + k
        push!(extra_bindings,
            "$(iname) = (length(x) >= $port_idx && !isnan(x[$port_idx])) ? x[$port_idx] : 0.0")
    end

    # Build derivative expressions
    deriv_exprs = String[]
    for (i, eq) in enumerate(equations)
        # Replace state variable references with _eff versions
        eq_eff = eq
        for var in state_vars
            eq_eff = replace(eq_eff, Regex("\\b$(var)\\b") => "$(var)_eff")
        end
        push!(deriv_exprs,
            "d$(state_vars[i]) = (length(x) >= $i && !isnan(x[$i])) ? 0.0 : ($eq_eff)")
    end

    # Assemble the full function body
    func_body = join(vcat(
        state_bindings,
        state_eff_bindings,
        param_bindings,
        extra_bindings,
        deriv_exprs,
        ["return [" * join(["d$(var)" for var in state_vars], ", ") * "]"]
    ), "\n    ")

    func_str = """
    function _custom_dynamics(u, x, p, t)
        $func_body
    end
    """

    # Compile the function
    func_expr = Meta.parse(func_str)
    dynamics_fn = Base.eval(CustomSystems, func_expr)

    # Readout: return all state variables
    readout_fn = (u, p, t) -> u

    machine = ContinuousMachine{Float64}(
        total_inputs,
        nstates,
        nstates,  # noutputs = nstates
        dynamics_fn,
        readout_fn
    )

    return machine
end

# =============================================================================
# Registration
# =============================================================================

"""
    validate_and_register_custom_system!(name, state_vars, equations, params, inputs, initial_state)

Validate expressions, build dynamics, and register as a template.
Returns (success::Bool, message::String).
"""
function validate_and_register_custom_system!(
    name::String,
    state_vars::Vector{String},
    equations::Vector{String},
    params::Vector{Tuple{String, Float64}},
    inputs::Vector{String},
    initial_state::Vector{Float64}
)
    # Validate name
    if isempty(name) || !occursin(r"^[a-zA-Z_][a-zA-Z0-9_]*$", name)
        return false, "Invalid system name: '$name'"
    end

    # Check state/equation count match
    if length(state_vars) != length(equations)
        return false, "Number of state variables ($(length(state_vars))) doesn't match number of equations ($(length(equations)))"
    end

    if length(initial_state) != length(state_vars)
        return false, "Number of initial values ($(length(initial_state))) doesn't match number of state variables ($(length(state_vars)))"
    end

    param_names = [p[1] for p in params]
    param_defaults = [p[2] for p in params]

    # Validate each equation
    for (i, eq) in enumerate(equations)
        err = validate_expression(eq, state_vars, param_names, inputs)
        if err !== nothing
            return false, "Equation $(state_vars[i]): $err"
        end
    end

    # Build the machine
    local machine
    try
        machine = build_custom_dynamics(state_vars, equations, param_names, param_defaults, inputs)
    catch e
        return false, "Failed to compile dynamics: $(sprint(showerror, e))"
    end

    # Test the machine with initial state
    nstates = length(state_vars)
    nparams = length(param_names)
    nextra = length(inputs)
    total_inputs = nstates + nparams + nextra
    try
        test_inputs = fill(NaN, total_inputs)
        result = eval_dynamics(machine, initial_state, test_inputs, nothing, 0.0)
        if length(result) != nstates
            return false, "Dynamics returned $(length(result)) values, expected $nstates"
        end
        # Check for NaN/Inf in output
        if any(isnan, result) || any(isinf, result)
            return false, "Dynamics produced NaN or Inf with initial conditions"
        end
    catch e
        return false, "Dynamics evaluation failed: $(sprint(showerror, e))"
    end

    # Build input names and defaults for the expanded port convention
    input_names = vcat(
        [v * "_in" for v in state_vars],
        param_names,
        inputs
    )
    input_defaults = vcat(
        fill(NaN, nstates),
        param_defaults,
        fill(0.0, nextra)
    )

    output_names = copy(state_vars)

    # Register constructor
    captured_machine = machine
    captured_initial = copy(initial_state)

    function constructor(p::Dict{String, Float64})
        # Rebuild with potentially updated parameters
        new_defaults = copy(param_defaults)
        for (i, pname) in enumerate(param_names)
            if haskey(p, pname)
                new_defaults[i] = p[pname]
            end
        end
        new_machine = build_custom_dynamics(state_vars, equations, param_names, new_defaults, inputs)
        return new_machine, copy(captured_initial)
    end

    # Ensure unique ID (prefix with "custom_")
    template_id = "custom_" * name

    template = SystemTemplate(
        template_id,
        "Custom: $name",
        nstates,
        total_inputs,
        nstates,
        [(pn, pd) for (pn, pd) in zip(param_names, param_defaults)],
        copy(state_vars),
        input_names,
        output_names,
        input_defaults,
        constructor
    )

    SYSTEM_REGISTRY[template_id] = template
    println("Registered custom system: $template_id ($nstates states, $total_inputs inputs)")

    return true, "Custom system '$name' registered successfully as '$template_id'"
end

end # module
