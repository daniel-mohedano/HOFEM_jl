module Interfaces

include.(filter(contains(r".jl$"), readdir(@__DIR__; join=true)))

end # module
