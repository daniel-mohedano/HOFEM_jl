using Test

@testset "Module Parsing - Regex" begin
  # Test module parsing
  parser = HOFEM_jl.RegexParserImpl()
  parsed_modules = HOFEM_jl.Parsing.parse(parser, [joinpath(RESOURCES_PATH, "example_mod.F90")])

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
    @test m.type isa HOFEM_jl.Parsing.IntrinsicType
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
  @test extended_type.members[1].type.name == "REAL"
  @test extended_type.members[1].type.kind == "8"
  @test extended_type.members[2].type isa HOFEM_jl.Parsing.DerivedType
  @test extended_type.members[2].type.name == "derived_type"

  # Test module variables (only captures derived type ones)
  @test length(mod.variables) == 1

  # Test global_var
  global_var = mod.variables[1]
  @test global_var.var.name == "global_var"
  @test global_var.var.type isa HOFEM_jl.Parsing.DerivedType
  @test global_var.var.type.name == "derived_type"
  @test global_var.visibility == HOFEM_jl.Parsing.Public

  # Test procedures
  @test length(mod.procedures) == 5

  # Test distance function
  distance = mod.procedures[1]
  @test distance.name == "distance"
  @test distance.is_pure == true
  @test distance.is_elemental == false
  @test length(distance.args) == 2
  for arg in distance.args
    @test arg.type isa HOFEM_jl.Parsing.DerivedType
    @test arg.type.name == "derived_type"
    @test lowercase(arg.attributes.intent) == "in"
  end
  @test distance.ret isa HOFEM_jl.Parsing.IntrinsicType
  @test distance.ret.name == "REAL"
  @test distance.ret.kind == "8"

  # Test other function
  other = mod.procedures[2]
  @test other.name == "other"
  @test other.is_pure == false
  @test other.is_elemental == false
  @test length(other.args) == 2
  for arg in other.args
    @test arg.type isa HOFEM_jl.Parsing.IntrinsicType
    @test arg.type.name == "REAL"
  end
  @test other.ret isa HOFEM_jl.Parsing.IntrinsicType
  @test other.ret.name == "INTEGER"

  # Test calc_excitation_vectorial_3D
  calc = mod.procedures[3]
  @test calc.name == "calc_excitation_vectorial_3D"
  @test length(calc.args) == 5


  # Test normalize subroutine
  normalize = mod.procedures[4]
  @test normalize.name == "normalize"
  @test normalize.is_pure == false
  @test normalize.is_elemental == true
  @test length(normalize.args) == 1
  @test normalize.args[1].type isa HOFEM_jl.Parsing.DerivedType
  @test normalize.args[1].type.name == "derived_type"
  @test lowercase(normalize.args[1].attributes.intent) == "inout"
  @test normalize.ret === nothing

  # Test f 
  f = mod.procedures[5]
  @test f.name == "f"
  @test length(f.args) == 2

end
