module Parsing

export Parser, parse

include("Parser.jl")
include("RegexParser/RegexParser.jl")
include("ASTParser/ASTParser.jl")

using .RegexParser
using .ASTParser

import .RegexParser: parse
import .ASTParser: parse

end
