module HOFEM_jl

include("Parsing/Parsing.jl")
using .Parsing

include("Generation/Generation.jl")
#include.(filter(contains(r".jl$"), readdir(joinpath(@__DIR__, "Interfaces"); join=true)))

include("MatrixMarket.jl/src/MatrixMarket.jl")
using .MatrixMarket

end
