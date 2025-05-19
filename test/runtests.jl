using HOFEM_jl
using Test

if isempty(ARGS) || "all" in ARGS
  all_tests = true
else
  all_tests = false
end

if all_tests || "regex" in ARGS
  include("module_parsing_regex.jl")
end

if all_tests || "ast" in ARGS
  include("module_parsing_ast.jl")
end
