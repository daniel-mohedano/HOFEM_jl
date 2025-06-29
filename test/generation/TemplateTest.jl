using Test

using .HOFEM_jl
using .HOFEM_jl.Parsing: IntrinsicType
using .HOFEM_jl.Generation: FORTRAN, JULIA, t_setter_name, t_getter_name, t_new_name, t_free_name,
  t_getter_module_var_name, t_type_print_name, t_procedure_wrapper_name, t_setter, t_getter,
  t_factory, t_getter_module_var, t_type_print

@testset "Template Functions" begin

  @testset "Naming Functions" begin
    # Test setter/getter naming
    @test t_setter_name("MyType", "field", FORTRAN) == "MyType_set_field"
    @test t_getter_name("MyType", "field", FORTRAN) == "MyType_get_field"
    @test t_setter_name("MyType", "field", JULIA) == "MyType_set_field!"
    @test t_getter_name("MyType", "field", JULIA) == "MyType_get_field"

    # Test factory function naming
    @test t_new_name("MyType") == "new_MyType"
    @test t_free_name("MyType") == "free_MyType"

    # Test module variable naming
    @test t_getter_module_var_name("global_var") == "get_global_var"

    # Test type print naming
    @test t_type_print_name("MyType") == "print_MyType"

    # Test procedure wrapper naming
    @test t_procedure_wrapper_name("my_func") == "jl_my_func"
  end

  @testset "Fortran Templates" begin
    int_type = IntrinsicType("integer", "4", nothing)

    # Test Fortran setter template
    setter_code = t_setter("TestType", "value", int_type, "INTEGER(C_INT)", FORTRAN)
    @test contains(setter_code, "SUBROUTINE TestType_set_value(data_c_ptr, val) BIND(C)")
    @test contains(setter_code, "TYPE(C_PTR), VALUE :: data_c_ptr")
    @test contains(setter_code, "INTEGER(C_INT), VALUE :: val")
    @test contains(setter_code, "TYPE(TestType), POINTER :: data")
    @test contains(setter_code, "CALL c_f_pointer(data_c_ptr, data)")
    @test contains(setter_code, "data%value = val")

    # Test Fortran getter template
    getter_code = t_getter("TestType", "value", int_type, "INTEGER(C_INT)", FORTRAN)
    @test contains(getter_code, "FUNCTION TestType_get_value(data_c_ptr) BIND(C)")
    @test contains(getter_code, "INTEGER(C_INT) :: TestType_get_value")
    @test contains(getter_code, "TestType_get_value = data%value")

    # Test Fortran factory template
    factory_code = t_factory("TestType", FORTRAN)
    @test contains(factory_code, "FUNCTION new_TestType() BIND(C)")
    @test contains(factory_code, "TYPE(C_PTR) :: new_TestType")
    @test contains(factory_code, "TYPE(TestType), POINTER :: obj")
    @test contains(factory_code, "ALLOCATE(obj)")
    @test contains(factory_code, "new_TestType = c_loc(obj)")
    @test contains(factory_code, "SUBROUTINE free_TestType(p) BIND(C)")
    @test contains(factory_code, "CALL c_f_pointer(p, obj)")
    @test contains(factory_code, "IF (ASSOCIATED(obj)) DEALLOCATE(obj)")

    # Test Fortran module variable getter
    mod_var_code = t_getter_module_var("test_var", "TestType", FORTRAN)
    @test contains(mod_var_code, "FUNCTION get_test_var() BIND(C)")
    @test contains(mod_var_code, "TYPE(C_PTR) :: get_test_var")
    @test contains(mod_var_code, "TYPE(TestType), POINTER :: ptr")
    @test contains(mod_var_code, "ptr => test_var")
    @test contains(mod_var_code, "get_test_var = c_loc(ptr)")
  end

  @testset "Julia Templates" begin
    int_type = IntrinsicType("integer", "4", nothing)

    # Test Julia setter template
    setter_code = t_setter("TestType", "value", int_type, "Cint", JULIA)
    @test contains(setter_code, "function set_value!(obj::TestType, val::Cint)")
    @test contains(setter_code, "@ccall _HOFEM_LIB_PATH.testtype_set_value(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid")

    # Test Julia getter template
    getter_code = t_getter("TestType", "value", int_type, "Cint", JULIA)
    @test contains(getter_code, "function get_value(obj::TestType)::Cint")
    @test contains(getter_code, "return @ccall _HOFEM_LIB_PATH.testtype_get_value(obj.handle::Ptr{Cvoid})::Cint")

    # Test Julia factory template
    factory_code = t_factory("TestType", JULIA)
    @test contains(factory_code, "struct TestType")
    @test contains(factory_code, "handle::Ptr{Cvoid}")
    @test contains(factory_code, "function TestType()")
    @test contains(factory_code, "return TestType(@ccall _HOFEM_LIB_PATH.new_testtype()::Ptr{Cvoid})")
    @test contains(factory_code, "function Base.finalize(obj::TestType)")
    @test contains(factory_code, "@ccall _HOFEM_LIB_PATH.free_testtype(obj.handle::Ptr{Cvoid})::Cvoid")

    # Test Julia module variable getter
    mod_var_code = t_getter_module_var("test_var", "TestType", JULIA)
    @test contains(mod_var_code, "function get_test_var()::TestType")
    @test contains(mod_var_code, "return TestType(@ccall _HOFEM_LIB_PATH.get_test_var()::Ptr{Cvoid})")

    # Test Julia type print template
    print_code = t_type_print("TestType", ["field1", "field2"], JULIA)
    @test contains(print_code, "function print_TestType(obj::TestType)")
    @test contains(print_code, "@ccall _HOFEM_LIB_PATH.print_testtype(obj.handle::Ptr{Cvoid})::Cvoid")
  end

  @testset "String Type Templates" begin
    char_type = IntrinsicType("character", nothing, "*")

    # Test string setter in Fortran
    string_setter_fortran = t_setter("TestType", "name", char_type, "CHARACTER(KIND=C_CHAR), DIMENSION(*)", FORTRAN)
    @test contains(string_setter_fortran, "CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val")
    @test contains(string_setter_fortran, "IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT")
    @test contains(string_setter_fortran, "data%name(i:i) = val(i)")

    # Test string setter in Julia
    string_setter_julia = t_setter("TestType", "name", char_type, "Ptr{Cchar}", JULIA)
    @test contains(string_setter_julia, "function set_name!(obj::TestType, val::String)")
    @test contains(string_setter_julia, "val::Ptr{Cchar}")

    # Test string getter in Fortran
    string_getter_fortran = t_getter("TestType", "name", char_type, "CHARACTER(KIND=C_CHAR), DIMENSION(*)", FORTRAN)
    @test contains(string_getter_fortran, "SUBROUTINE TestType_get_name(data_c_ptr, string) BIND(C)")
    @test contains(string_getter_fortran, "CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string")
    @test contains(string_getter_fortran, "string(i) = data%name(i:i)")
    @test contains(string_getter_fortran, "string(i) = C_NULL_CHAR")
  end
end
