using Test
using HOFEM_jl

RESOURCES_PATH = joinpath(@__DIR__, "resources/")

if isempty(ARGS) || "all" in ARGS
  all_tests = true
else
  all_tests = false
end

if all_tests || "regex" in ARGS
  println("Executing RegexParserImplTest.jl")
  include("parsing/RegexParserImplTest.jl")
end

if all_tests || "ast" in ARGS
  println("Executing ASTParserImplTest.jl")
  include("parsing/ASTParserImplTest.jl")
end
