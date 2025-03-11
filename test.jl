ENV["HOFEM_LIB_PATH"] = "/mnt/c/Users/danie/Desktop/uc3m/HOFEM-AIRBUS_Julia_interface/source/lib/libHOFEM_MUMPS_LP64_DYNAMIC_LINUX_d.so"
include("src/HOFEM_jl.jl")
using .HOFEM_jl

using LinearAlgebra

matrix_name = "resources/matrices/single/one_tet_FEM_matrix_RHSblock_1_MPIrank0"
matrix_rhs_name = "resources/matrices/single/one_tet_FEM_matrix_RHSblock_1_MPIrank.rhs"

A = HOFEM_jl.mmread(matrix_name)
b = HOFEM_jl.mmread(matrix_rhs_name)

@info("Information on A: ", A.m, A.n, A[1, 1], A[A.m, A.n])
@info("Information on b: ", size(b), b[1, 1], b[size(b)[1]])

if length(ARGS) == 0
    error("No project provided to the script")
end

# 1. Call initiate_parallel_environment
HOFEM_jl.initiate_parallel_environment()

# 2. Set up commonProperties 
project_name = ARGS[1]
mesh_filename = strip(project_name) * ".em.mesh"
commonProperties = HOFEM_jl.get_commonProperties()
HOFEM_jl.programCommonData_set_projectName!(commonProperties, project_name)
HOFEM_jl.programCommonData_set_mesh_filename!(commonProperties, mesh_filename)
HOFEM_jl.programCommonData_set_mesh_format!(commonProperties, "BINARY")
HOFEM_jl.programCommonData_set_geometry_units!(commonProperties, "mm")
HOFEM_jl.print_programCommonData(commonProperties)
#@ccall "./lib/libHOFEM_MUMPS_LP64_DYNAMIC_LINUX_d.so".test_common_properties()::Cvoid

# 3. Set up formulation terms
formulation_terms = HOFEM_jl.get_formulation_terms()
HOFEM_jl.FormulationTerms_set_physics!(formulation_terms, "wave_equation")
HOFEM_jl.FormulationTerms_set_basis_functions!(formulation_terms, "nedelec")
HOFEM_jl.FormulationTerms_set_variational_unknown!(formulation_terms, "electric_field")
#FormulationTerms_print(formulation_terms)

# 4. Set up some mesh object variables
global_mesh = HOFEM_jl.get_global_mesh()
HOFEM_jl.MeshObject_set_geo_order!(global_mesh, Cint(2))
HOFEM_jl.MeshObject_set_var_order!(global_mesh, Cint(2))

# 5. Read and create global FEM mesh by calling 
HOFEM_jl.mesh_interface_constructor!(global_mesh, mesh_filename)
HOFEM_jl.print_MeshObject(global_mesh)

# Get num_rhs
num_rhs = HOFEM_jl.programCommonData_get_numRHS(commonProperties)
coefficients = HOFEM_jl.read_coefficients_from_postprocess_file(1, 1, num_rhs, 98, project_name)
@info("Relative error:", HOFEM_jl.rel_error(A \ b, coefficients))
#@info("Test result: ", LinearAlgebra.norm((A * coefficients) - b))
#@info("A", A)
#@info("b", b)
#@info("Julia result: ", (A \ b))
#@info("Fortran coefficients", coefficients)

#HOFEM_jl.mmwrite("K.mtx", A)

#println("%%MatrixMarket matrix array complex general")
#println("98      1")
#for i = 1:size(b)[1]
#  println(real(b[i]), "       ", imag(b[i]))
#end
