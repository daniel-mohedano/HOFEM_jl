module HOFEM_jl

include("parsing/Parsing.jl")
using .Parsing

include("generation/Generation.jl")
#include.(filter(contains(r".jl$"), readdir(joinpath(@__DIR__, "Interfaces"); join=true)))

include("MatrixMarket.jl/src/MatrixMarket.jl")
using .MatrixMarket

end # module
