module HOFEM_jl

include("parsing/Parsing.jl")
include("generation/Generation.jl")
#include.(filter(contains(r".jl$"), readdir(joinpath(@__DIR__, "Interfaces"); join=true)))
include("MatrixMarket.jl/src/MatrixMarket.jl")
include("Utils.jl")

using .Parsing
using .Generation
using .MatrixMarket

end # module
