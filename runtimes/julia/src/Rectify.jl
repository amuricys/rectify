# Main module for Rectify Julia backend
module Rectify

include("Systems.jl")
include("World.jl")
include("Composition.jl")
include("Simulation.jl")
include("Server.jl")

using .Systems
using .World
using .Composition
using .Simulation
using .Server

export start_server

end # module
