module Parsing

export Parser, ASTParserImpl, RegexParserImpl, parse

include("ParserTypes.jl")
include("RegexParser/RegexParserImpl.jl")
include("ASTParser/ASTParserImpl.jl")

end
