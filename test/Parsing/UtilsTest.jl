using Test

using .HOFEM_jl
using .HOFEM_jl.Parsing: IntrinsicType, DerivedType, Variable, VariableAttrs, Procedure, Public,
  fortran_type, julia_type, isstring, needs_wrapper, get_type, get_var_attributes, get_type_attributes

@testset "Parsing Utils Tests" begin

  @testset "test fortran_type" begin
    # Test case 1: Basic intrinsic types
    int_type = IntrinsicType("integer", "4", nothing)
    @test fortran_type(int_type) == "INTEGER(C_INT32_T)"

    real_type = IntrinsicType("real", "8", nothing)
    @test fortran_type(real_type) == "REAL(C_DOUBLE)"

    # Test case 2: Character types with length
    char_type = IntrinsicType("character", nothing, "*")
    @test fortran_type(char_type) == "CHARACTER(KIND=C_CHAR), DIMENSION(*)"

    char_fixed = IntrinsicType("character", nothing, "10")
    @test fortran_type(char_fixed) == "CHARACTER(KIND=C_CHAR), DIMENSION(*)"

    # Test case 3: Logical type
    logical_type = IntrinsicType("logical", nothing, nothing)
    @test fortran_type(logical_type) == "LOGICAL(C_BOOL)"

    # Test case 4: Unknown type defaults
    unknown_type = IntrinsicType("unknown", nothing, nothing)
    @test fortran_type(unknown_type) === nothing
  end

  @testset "test julia_type" begin
    # Test case 1: Basic intrinsic types
    int_type = IntrinsicType("integer", "4", nothing)
    @test julia_type(int_type) == "Cint"

    real_type = IntrinsicType("real", "8", nothing)
    @test julia_type(real_type) == "Cdouble"

    # Test case 2: Character types
    char_type = IntrinsicType("character", nothing, "*")
    @test julia_type(char_type) == "Ptr{Cchar}"

    # Test case 3: Logical type
    logical_type = IntrinsicType("logical", nothing, nothing)
    @test julia_type(logical_type) == "Cuchar"

    # Test case 4: All integer types map to Cint
    int8_type = IntrinsicType("integer", "1", nothing)
    @test julia_type(int8_type) == "Cint"

    int16_type = IntrinsicType("integer", "2", nothing)
    @test julia_type(int16_type) == "Cint"

    int64_type = IntrinsicType("integer", "8", nothing)
    @test julia_type(int64_type) == "Cint"

    # Test case 5: Different real kinds
    real32_type = IntrinsicType("real", "4", nothing)
    @test julia_type(real32_type) == "Cfloat"

    # Test case 6: Unknown type defaults
    unknown_type = IntrinsicType("unknown", nothing, nothing)
    @test julia_type(unknown_type) == "Ptr{Cvoid}"
  end

  @testset "test isstring" begin
    # Test case 1: Character types are strings
    char_type = IntrinsicType("character", nothing, "*")
    @test isstring(char_type) == true

    char_fixed = IntrinsicType("character", nothing, "10")
    @test isstring(char_fixed) == true

    char_no_len = IntrinsicType("character", nothing, nothing)
    @test isstring(char_no_len) == false  # needs len to be considered string

    # Test case 2: Non-character types are not strings
    int_type = IntrinsicType("integer", "4", nothing)
    @test isstring(int_type) == false

    real_type = IntrinsicType("real", "8", nothing)
    @test isstring(real_type) == false

    logical_type = IntrinsicType("logical", nothing, nothing)
    @test isstring(logical_type) == false

    # Test case 3: Case insensitive matching
    char_upper = IntrinsicType("CHARACTER", nothing, "*")
    @test isstring(char_upper) == true

    char_mixed = IntrinsicType("Character", nothing, "*")
    @test isstring(char_mixed) == true
  end

  @testset "test needs_wrapper" begin
    # Test case 1: Procedure with no special arguments doesn't need wrapper
    simple_proc = Procedure(
      "simple_func",
      [Variable("x", IntrinsicType("real", "8", nothing), VariableAttrs())],
      Variable("result", IntrinsicType("real", "8", nothing), VariableAttrs()),
      false, false, false, Public
    )
    @test needs_wrapper(simple_proc) == false

    # Test case 2: Procedure with array arguments needs wrapper
    array_proc = Procedure(
      "array_func",
      [Variable("arr", IntrinsicType("real", "8", nothing), VariableAttrs(false, false, false, false, false, ["10"], nothing))],
      nothing,
      false, false, false, Public
    )
    @test needs_wrapper(array_proc) == true

    # Test case 3: Procedure with derived type arguments needs wrapper
    derived_proc = Procedure(
      "derived_func",
      [Variable("obj", DerivedType("Point2D"), VariableAttrs())],
      nothing,
      false, false, false, Public
    )
    @test needs_wrapper(derived_proc) == true

    # Test case 4: BIND(C) procedure with character array doesn't need wrapper
    bind_c_char_proc = Procedure(
      "bind_c_char_func",
      [Variable("str", IntrinsicType("character", nothing, "*"), VariableAttrs(false, false, false, false, false, ["*"], nothing))],
      nothing,
      false, false, true, Public  # is_bind_c = true
    )
    @test needs_wrapper(bind_c_char_proc) == false

    # Test case 5: Non-BIND(C) procedure with character array needs wrapper
    non_bind_c_char_proc = Procedure(
      "non_bind_c_char_func",
      [Variable("str", IntrinsicType("character", nothing, "*"), VariableAttrs(false, false, false, false, false, ["*"], nothing))],
      nothing,
      false, false, false, Public  # is_bind_c = false
    )
    @test needs_wrapper(non_bind_c_char_proc) == true

    # Test case 6: Mixed arguments - needs wrapper if any argument requires it
    mixed_proc = Procedure(
      "mixed_func",
      [
        Variable("x", IntrinsicType("real", "8", nothing), VariableAttrs()),
        Variable("obj", DerivedType("Point2D"), VariableAttrs())
      ],
      nothing,
      false, false, false, Public
    )
    @test needs_wrapper(mixed_proc) == true
  end

  @testset "test get_type" begin
    # Test case 1: Procedure types should return nothing
    @test get_type("procedure") === nothing
    @test get_type("PROCEDURE") === nothing
    @test get_type("Procedure") === nothing

    # Test case 2: Derived types with TYPE() syntax
    derived_result = get_type("TYPE(Point2D)")
    @test derived_result isa DerivedType
    @test derived_result.name == "Point2D"

    derived_lower = get_type("type(mytype)")
    @test derived_lower isa DerivedType
    @test derived_lower.name == "mytype"

    # Test case 3: Class types with CLASS() syntax
    class_result = get_type("CLASS(BaseClass)")
    @test class_result isa DerivedType
    @test class_result.name == "BaseClass"

    # Test case 4: Intrinsic types with kind
    int_with_kind = get_type("INTEGER(KIND=4)")
    @test int_with_kind isa IntrinsicType
    @test int_with_kind.name == "INTEGER"
    @test int_with_kind.kind == "4"

    real_with_kind = get_type("REAL(KIND=8)")
    @test real_with_kind isa IntrinsicType
    @test real_with_kind.name == "REAL"
    @test real_with_kind.kind == "8"

    # Test case 5: Character types with length
    char_with_len = get_type("CHARACTER(LEN=10)")
    @test char_with_len isa IntrinsicType
    @test char_with_len.name == "CHARACTER"
    @test char_with_len.len == "10"

    # Test case 6: Simple intrinsic types
    simple_int = get_type("INTEGER")
    @test simple_int isa IntrinsicType
    @test simple_int.name == "INTEGER"
    @test simple_int.kind === nothing

    simple_real = get_type("REAL")
    @test simple_real isa IntrinsicType
    @test simple_real.name == "REAL"

    # Test case 7: Case insensitive matching
    mixed_case = get_type("Integer")
    @test mixed_case isa IntrinsicType
    @test mixed_case.name == "Integer"
  end

  @testset "test get_var_attributes" begin
    # Test case 1: No attributes
    attrs = get_var_attributes("")
    @test attrs.is_parameter == false
    @test attrs.is_allocatable == false
    @test attrs.is_pointer == false
    @test attrs.is_target == false
    @test attrs.dimensions === nothing

    # Test case 2: Single attributes
    param_attrs = get_var_attributes(", PARAMETER")
    @test param_attrs.is_parameter == true
    @test param_attrs.is_allocatable == false

    alloc_attrs = get_var_attributes(", ALLOCATABLE")
    @test alloc_attrs.is_allocatable == true
    @test alloc_attrs.is_parameter == false

    # Test case 3: Multiple attributes
    multi_attrs = get_var_attributes(", PARAMETER, ALLOCATABLE")
    @test multi_attrs.is_parameter == true
    @test multi_attrs.is_allocatable == true

    # Test case 4: Dimension parsing
    dim_attrs = get_var_attributes(", DIMENSION(10)")
    @test dim_attrs.dimensions == ["10"]

    multi_dim_attrs = get_var_attributes(", DIMENSION(10, 20)")
    @test multi_dim_attrs.dimensions == ["10", "20"]

    var_dim_attrs = get_var_attributes(", DIMENSION(:, 1:10)")
    @test var_dim_attrs.dimensions == [":", "1:10"]

    # Test case 5: Case insensitive and whitespace handling
    messy_attrs = get_var_attributes("  ,  parameter  ,  ALLOCATABLE  ,  DIMENSION(5,:) ")
    @test messy_attrs.is_parameter == true
    @test messy_attrs.is_allocatable == true
    @test messy_attrs.dimensions == ["5", ":"]

    # Test case 6: Intent parsing
    intent_in_attrs = get_var_attributes(", INTENT(IN)")
    @test intent_in_attrs.intent == "in"

    intent_out_attrs = get_var_attributes(", INTENT(OUT)")
    @test intent_out_attrs.intent == "out"

    intent_inout_attrs = get_var_attributes(", INTENT(INOUT)")
    @test intent_inout_attrs.intent == "inout"
  end
end
