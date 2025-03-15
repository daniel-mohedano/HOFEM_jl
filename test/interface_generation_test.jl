using HOFEM_jl
using Test


@testset "Interface Generation" begin
  # Test module parsing
  lines = readlines("resources/example_mod.F90")
  mod = HOFEM_jl._parse(lines)

  @test mod !== nothing
  @test mod.name == "example_mod"

  # Test types parsing
  @test length(mod.types) == 2

  # Test derived_type
  base = mod.types[1]
  @test base.name == "derive_type"
  @test base.attributes.is_abstract == true
  @test base.attributes.extends === nothing
  @test base.attributes.is_public == false
  @test length(base.members) == 2
  @test all(m -> m.type isa HOFEM_jl.FIntrinsic && m.type.name == "REAL" && m.type.kind == "8", base.members)

  # Test extended_type
  derived = mod.types[2]
  @test derived.name == "extended_type"
  @test derived.attributes.is_abstract == false
  @test derived.attributes.extends == "derived_type"
  @test derived.attributes.is_public == true
  @test length(derived.members) == 1
  @test derived.members[1].attributes.is_allocatable == true
  @test derived.members[1].attributes.dimensions == [":"]

  # Test module variables
  @test length(mod.variables) == 2

  # Test global_var
  global_var = mod.variables[1]
  @test global_var.var.name == "global_var"
  @test global_var.var.type isa HOFEM_jl.FDerived
  @test global_var.var.type.name == "derived_type"
  @test global_var.visibility == HOFEM_jl.Public

  # Test pi constant
  pi_var = mod.variables[2]
  @test pi_var.var.name == "pi"
  @test pi_var.var.type isa HOFEM_jl.FIntrinsic
  @test pi_var.var.type.name == "REAL"
  @test pi_var.var.type.kind == "8"
  @test pi_var.var.attributes.is_parameter == true

  # Test procedures
  @test length(mod.procedures) == 2

  # Test distance function
  distance = mod.procedures[1]
  @test distance.name == "distance"
  @test distance.is_pure == true
  @test distance.is_elemental == false
  @test length(distance.args) == 2
  @test all(arg -> arg.type isa HOFEM_jl.FDerived && arg.type.name == "derived_type", distance.args)
  @test all(arg -> arg.attributes.intent == :in, distance.args)
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
  @test normalize.args[1].attributes.intent == :inout
  @test normalize.ret === nothing
end
