#!/usr/bin/env julia

# Activate the project environment
using Pkg
Pkg.activate(@__DIR__)

include("src/Server.jl")
using .Server

start_server(port=8082)
