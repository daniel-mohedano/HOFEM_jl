using Test

using .HOFEM_jl
using .HOFEM_jl.Parsing: IntrinsicType, DerivedType, Variable, VariableAttrs, ModuleVariable, Procedure, Public
using .HOFEM_jl.Generation: build_member_access, build_module_var_access, find_custom_section,
  find_fortran_routines_custom_section, find_julia_routines_custom_section, FORTRAN, JULIA

@testset "Generation Utils Tests" begin

  @testset "test build_member_access" begin
    # Test case 1: Skip derived type members
    derived_type_member = Variable(
      "derived_member",
      DerivedType("SomeType"),
      VariableAttrs()
    )

    result = build_member_access("TestType", derived_type_member, String[], FORTRAN)
    @test result == ""

    result = build_member_access("TestType", derived_type_member, String[], JULIA)
    @test result == ""

    # Test case 2: Skip array members
    array_member = Variable(
      "array_member",
      IntrinsicType("integer", "4", nothing),
      VariableAttrs(false, false, false, false, false, ["10"], nothing)
    )

    result = build_member_access("TestType", array_member, String[], FORTRAN)
    @test result == ""

    result = build_member_access("TestType", array_member, String[], JULIA)
    @test result == ""

    # Test case 3: Generate code for valid intrinsic scalar members
    scalar_member = Variable(
      "scalar_member",
      IntrinsicType("real", "8", nothing),
      VariableAttrs()
    )

    result = build_member_access("TestType", scalar_member, String[], FORTRAN)
    @test result != ""
    @test contains(result, "TestType_set_scalar_member")
    @test contains(result, "TestType_get_scalar_member")
    @test contains(result, "BIND(C)")

    result = build_member_access("TestType", scalar_member, String[], JULIA)
    @test result != ""
    @test contains(result, "set_scalar_member!")
    @test contains(result, "get_scalar_member")
    @test contains(result, "@ccall")

    # Test case 4: Skip generation when custom routines already exist
    custom_routines = ["TestType_set_scalar_member", "TestType_get_scalar_member"]
    result = build_member_access("TestType", scalar_member, custom_routines, FORTRAN)
    @test result == ""

    # Test case 5: Generate only missing routines
    partial_custom_routines = ["TestType_set_scalar_member"]
    result = build_member_access("TestType", scalar_member, partial_custom_routines, FORTRAN)
    @test result != ""
    @test !contains(result, "TestType_set_scalar_member")  # Should skip setter
    @test contains(result, "TestType_get_scalar_member")   # Should generate getter
  end

  @testset "test build_module_var_access" begin
    # Test case 1: Skip non-target variables
    non_target_var = ModuleVariable(
      Variable(
        "non_target",
        DerivedType("Point2D"),
        VariableAttrs()  # is_target = false
      ),
      Public
    )

    result = build_module_var_access(non_target_var, String[], FORTRAN)
    @test result == ""

    result = build_module_var_access(non_target_var, String[], JULIA)
    @test result == ""

    # Test case 2: Generate code for target variables
    target_var = ModuleVariable(
      Variable(
        "target_var",
        DerivedType("Point2D"),
        VariableAttrs(false, false, false, true, false, nothing, nothing)  # is_target = true
      ),
      Public
    )

    result = build_module_var_access(target_var, String[], FORTRAN)
    @test result != ""
    @test contains(result, "get_target_var")
    @test contains(result, "BIND(C)")

    result = build_module_var_access(target_var, String[], JULIA)
    @test result != ""
    @test contains(result, "get_target_var")
    @test contains(result, "@ccall")

    # Test case 3: Skip generation when custom routine exists
    custom_routines = ["get_target_var"]
    result = build_module_var_access(target_var, custom_routines, FORTRAN)
    @test result == ""
  end

  @testset "test find_custom_section" begin
    # Test case 1: Non-existent file returns default custom section
    custom_section_fortran = find_custom_section("non_existent.F90", FORTRAN)
    @test contains(custom_section_fortran, "!@C")
    @test contains(custom_section_fortran, "!/@C")
    @test contains(custom_section_fortran, "Anything inside this section will be preserved")

    custom_section_julia = find_custom_section("non_existent.jl", JULIA)
    @test contains(custom_section_julia, "#@C")
    @test contains(custom_section_julia, "#/@C")
    @test contains(custom_section_julia, "Anything inside this section will be preserved")

    # Test case 2: File with custom section
    temp_file = tempname() * ".F90"
    try
      write(
        temp_file,
        """
MODULE test_mod
CONTAINS
!@C Custom section begin
SUBROUTINE my_custom_routine()
    ! Custom implementation
END SUBROUTINE
!/@C
END MODULE
"""
      )

      custom_section = find_custom_section(temp_file, FORTRAN)
      @test contains(custom_section, "my_custom_routine")
      @test contains(custom_section, "Custom implementation")
      @test contains(custom_section, "!@C")
      @test contains(custom_section, "!/@C")
    finally
      isfile(temp_file) && rm(temp_file)
    end

    # Test case 3: File without custom section
    temp_file = tempname() * ".F90"
    try
      write(
        temp_file,
        """
MODULE test_mod
CONTAINS
SUBROUTINE regular_routine()
END SUBROUTINE
END MODULE
"""
      )

      custom_section = find_custom_section(temp_file, FORTRAN)
      @test contains(custom_section, "!@C")
      @test contains(custom_section, "!/@C")
      @test !contains(custom_section, "regular_routine")
    finally
      isfile(temp_file) && rm(temp_file)
    end
  end

  @testset "test find_fortran_routines_custom_section" begin
    # Test case 1: Extract subroutine names
    custom_section = """
    !@C Custom section
    SUBROUTINE my_sub() BIND(C)
    END SUBROUTINE

    FUNCTION my_func() BIND(C)
    END FUNCTION

    ! Comment line
    SUBROUTINE another_sub(x) BIND(C)
    END SUBROUTINE
    !/@C
    """

    routines = find_fortran_routines_custom_section(custom_section)
    @test "my_sub" in routines
    @test "my_func" in routines
    @test "another_sub" in routines
    @test length(routines) == 3

    # Test case 2: No routines in section
    empty_section = """
    !@C Custom section
    ! Just comments
    ! No actual routines
    !/@C
    """

    routines = find_fortran_routines_custom_section(empty_section)
    @test isempty(routines)

    # Test case 3: Mixed case and whitespace handling
    mixed_section = """
    !@C Custom section
       SUBROUTINE   whitespace_sub  ()  BIND(C)
    END SUBROUTINE

    function lowercase_func() bind(c)
    end function
    !/@C
    """

    routines = find_fortran_routines_custom_section(mixed_section)
    @test "whitespace_sub" in routines
    @test "lowercase_func" in routines
  end

  @testset "test find_julia_routines_custom_section" begin
    # Test case 1: Extract function names
    custom_section = """
    #@C Custom section
    function my_function()
        return 42
    end

    function another_function(x, y)
        return x + y
    end

    # Comment line
    function third_function()
        # Implementation
    end
    #/@C
    """

    routines = find_julia_routines_custom_section(custom_section)
    @test "my_function" in routines
    @test "another_function" in routines
    @test "third_function" in routines
    @test length(routines) == 3

    # Test case 2: No functions in section
    empty_section = """
    #@C Custom section
    # Just comments
    # No actual functions
    #/@C
    """

    routines = find_julia_routines_custom_section(empty_section)
    @test isempty(routines)

    # Test case 3: Functions with complex signatures
    complex_section = """
    #@C Custom section
    function complex_function(obj::MyType, x::Float64)::Int32
        return 1
    end

    function generic_function(T::Type{<:Number})
        return T(0)
    end
    #/@C
    """

    routines = find_julia_routines_custom_section(complex_section)
    @test "complex_function" in routines
    @test "generic_function" in routines
  end
end
