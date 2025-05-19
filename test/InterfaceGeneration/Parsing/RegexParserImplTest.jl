using Test

include("../src/InterfaceGeneration/Parsing/RegexParser/RegexParser.jl")
using ..RegexParser

@testset "Module Parsing - Regex" failfast = true begin
  # Test module parsing
  parser = RegexParserImpl()
  parsed_modules = parse(parser, [joinpath(@__DIR__, "resources/example_mod.F90")])

  @test length(parsed_modules) == 1

  mod = parsed_modules[1]
  @test mod !== nothing
  @test mod.name == "example_mod"

  # Test types parsing
  @test length(mod.types) == 2

  # Test derived_type
  derived_type = mod.types[1]
  @test derived_type.name == "derived_type"
  @test derived_type.attributes.is_abstract == true
  @test derived_type.attributes.extends === nothing
  @test derived_type.attributes.is_public == false
  @test length(derived_type.members) == 2
  for m in derived_type.members
    @test m.type isa HOFEM_jl.FIntrinsic
    @test m.type.name == "REAL"
    @test m.type.kind == "8"
  end

  # Test extended_type
  extended_type = mod.types[2]
  @test extended_type.name == "extended_type"
  @test extended_type.attributes.is_abstract == false
  @test extended_type.attributes.extends == "derived_type"
  @test extended_type.attributes.is_public == true
  @test length(extended_type.members) == 2
  @test extended_type.members[1].attributes.is_allocatable == true
  @test extended_type.members[1].attributes.dimensions == [":"]
  @test extended_type.members[2].type isa HOFEM_jl.FDerived
  @test extended_type.members[2].type.name == "derived_type"

  # Test module variables (only captures derived type ones)
  @test length(mod.variables) == 1

  # Test global_var
  global_var = mod.variables[1]
  @test global_var.var.name == "global_var"
  @test global_var.var.type isa HOFEM_jl.FDerived
  @test global_var.var.type.name == "derived_type"
  @test global_var.visibility == HOFEM_jl.Public

  # Test procedures
  @test length(mod.procedures) == 2

  # Test distance function
  distance = mod.procedures[1]
  @test distance.name == "distance"
  @test distance.is_pure == true
  @test distance.is_elemental == false
  @test length(distance.args) == 2
  for arg in distance.args
    @test arg.type isa HOFEM_jl.FDerived
    @test arg.type.name == "derived_type"
    @test lowercase(arg.attributes.intent) == "in"
  end
  @test distance.ret isa HOFEM_jl.FIntrinsic
  @test distance.ret.name == "REAL"
  @test distance.ret.kind == "8"

  # Test normalize subroutine
  normalize = mod.procedures[2]
  @test normalize.name == "normalize"
  @test normalize.is_pure == false
  @test normalize.is_elemental == true
  @test length(normalize.args) == 1
  @test normalize.args[1].type isa HOFEM_jl.FDerived
  @test normalize.args[1].type.name == "derived_type"
  @test lowercase(normalize.args[1].attributes.intent) == "inout"
  @test normalize.ret === nothing
end
