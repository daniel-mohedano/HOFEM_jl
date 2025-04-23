module HOFEM_jl

include("InterfaceGeneration/InterfaceGeneration.jl")
include.(filter(contains(r".jl$"), readdir(joinpath(@__DIR__, "Interfaces"); join=true)))
include("MatrixMarket.jl/src/MatrixMarket.jl")

using .InterfaceGeneration
using .MatrixMarket

end #module
