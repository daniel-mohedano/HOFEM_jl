module HOFEM_jl

include("parsing/Parsing.jl")
include("generation/Generation.jl")
include("interfaces/Interfaces.jl")
include("MatrixMarket.jl/src/MatrixMarket.jl")
include("Utils.jl")

using .Parsing
using .Generation
using .Interfaces
using .MatrixMarket

end # module
