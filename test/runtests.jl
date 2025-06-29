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

if all_tests || "parsing" in ARGS
  println("Executing Parsing UtilsTest.jl")
  include("parsing/UtilsTest.jl")
end

if all_tests || "generation" in ARGS
  println("Executing TemplateTest.jl")
  include("generation/TemplateTest.jl")
  println("Executing Generation UtilsTest.jl")
  include("generation/UtilsTest.jl")
end

if all_tests || "e2e" in ARGS
  println("Executing EndToEndTest.jl")
  include("EndToEndTest.jl")
end
