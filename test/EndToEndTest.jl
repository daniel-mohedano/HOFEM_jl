using Test

using .HOFEM_jl
using .HOFEM_jl.Parsing: RegexParserImpl, parse
using .HOFEM_jl.Generation: generate_interfaces

@testset "End-to-End Pipeline Tests" begin
  # Parse the test module first
  parser = RegexParserImpl()
  parsed_modules = parse(parser, [joinpath(RESOURCES_PATH, "simple_mod.F90")])

  @test length(parsed_modules) == 1
  test_module = parsed_modules[1]

  @testset "Parsing Validation" begin
    # Validate that parsing worked correctly
    @test test_module.name == "simple_mod"
    @test length(test_module.types) == 1
    @test length(test_module.variables) == 1
    @test length(test_module.procedures) == 3

    # Validate derived type structure
    point2d_type = test_module.types[1]
    @test point2d_type.name == "Point2D"
    @test length(point2d_type.members) == 2
    @test point2d_type.members[1].name == "x"
    @test point2d_type.members[2].name == "y"

    # Validate module variable
    origin_var = test_module.variables[1]
    @test origin_var.var.name == "origin"
    @test origin_var.var.type.name == "Point2D"
    @test origin_var.var.attributes.is_target == true

    # Validate procedures
    procedure_names = [proc.name for proc in test_module.procedures]
    @test "distance" in procedure_names
    @test "normalize" in procedure_names
    @test "dot_product" in procedure_names
  end

  @testset "Complete Interface Generation Pipeline" begin
    # Create temporary directory for testing
    temp_dir = mktempdir()

    try
      # Test the complete pipeline: parsing -> generation
      generate_interfaces(parsed_modules, temp_dir)

      # Check that both interface files were created
      fortran_interface = joinpath(temp_dir, "simple_mod_jl_interface.F90")
      julia_interface = joinpath(temp_dir, "simple_mod_jl_interface.jl")

      @test isfile(fortran_interface)
      @test isfile(julia_interface)

      # Validate Fortran interface contains expected elements
      fortran_content = read(fortran_interface, String)
      @test contains(fortran_content, "MODULE simple_mod_jl_interface")
      @test contains(fortran_content, "USE simple_mod")
      @test contains(fortran_content, "iso_c_binding")
      @test contains(fortran_content, "BIND(C)")

      # Derived type interface functions
      @test contains(fortran_content, "Point2D_set_x")
      @test contains(fortran_content, "Point2D_get_x")
      @test contains(fortran_content, "Point2D_set_y")
      @test contains(fortran_content, "Point2D_get_y")
      @test contains(fortran_content, "new_Point2D")
      @test contains(fortran_content, "free_Point2D")
      @test contains(fortran_content, "print_Point2D")

      # Module variable access
      @test contains(fortran_content, "get_origin")

      # Procedure wrappers
      @test contains(fortran_content, "jl_distance")
      @test contains(fortran_content, "jl_normalize")
      @test contains(fortran_content, "jl_dot_product")

      # Validate Julia interface contains expected elements
      julia_content = read(julia_interface, String)
      @test contains(julia_content, "@ccall")
      @test contains(julia_content, "_HOFEM_LIB_PATH")

      # Derived type struct and constructor
      @test contains(julia_content, "struct Point2D")
      @test contains(julia_content, "handle::Ptr{Cvoid}")
      @test contains(julia_content, "function Point2D()")
      @test contains(julia_content, "function Base.finalize(obj::Point2D)")

      # Derived type interface functions
      @test contains(julia_content, "function set_x!(obj::Point2D")
      @test contains(julia_content, "function get_x(obj::Point2D")
      @test contains(julia_content, "function set_y!(obj::Point2D")
      @test contains(julia_content, "function get_y(obj::Point2D")
      @test contains(julia_content, "function print_Point2D(obj::Point2D)")

      # Module variable access
      @test contains(julia_content, "function get_origin()")

      # Procedure calls
      @test contains(julia_content, "function distance(")
      @test contains(julia_content, "function normalize(")
      @test contains(julia_content, "function dot_product(")

    finally
      # Clean up temporary directory
      rm(temp_dir, recursive=true)
    end
  end

  @testset "Error Handling" begin
    # Test with invalid output path
    invalid_path = "/non/existent/path"

    # Should not throw but should warn
    @test_logs (:warn, r"Invalid interface path") generate_interfaces(parsed_modules, invalid_path)

    # Test with empty modules list
    empty_modules = typeof(test_module)[]
    temp_dir = mktempdir()

    try
      generate_interfaces(empty_modules, temp_dir)
      # Should complete without error, but no files should be created
      @test isempty(readdir(temp_dir))
    finally
      rm(temp_dir, recursive=true)
    end
  end

  @testset "Multiple Modules" begin
    # Test with multiple modules (using the same module twice for simplicity)
    multiple_modules = [test_module, test_module]
    temp_dir = mktempdir()

    try
      generate_interfaces(multiple_modules, temp_dir)

      # Should create interface files for each module
      # Since we're using the same module twice, we should still only get one set of files
      # (the second will overwrite the first)
      fortran_files = filter(f -> endswith(f, ".F90"), readdir(temp_dir))
      julia_files = filter(f -> endswith(f, ".jl"), readdir(temp_dir))

      @test length(fortran_files) == 1
      @test length(julia_files) == 1
      @test "simple_mod_jl_interface.F90" in fortran_files
      @test "simple_mod_jl_interface.jl" in julia_files

    finally
      rm(temp_dir, recursive=true)
    end
  end
end
