module Interfaces

# TODO: automatic loading with dependency resolution
interface_files = [
  "fake_SparseMatrix_mod_jl_interface.jl",
  "fake_MUMPS_driver_mod_jl_interface.jl",
  "fake_HOFEM_library_mod_jl_interface.jl",
  "formulation_terms_module_jl_interface.jl",
  "mesh_interface_module_jl_interface.jl",
  "mesh_object_module_jl_interface.jl",
  "postprocess_coefficients_module_jl_interface.jl",
  "program_parallel_module_jl_interface.jl",
  "program_variables_module_jl_interface.jl"
]
include.(interface_files)

end # module
