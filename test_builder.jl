include("src/HOFEM_jl.jl")
using .HOFEM_jl
using JSON3

#HOFEM_jl.generate_interfaces([
#    "../program_variables_module.F90", 
#    "../formulation_terms_module.F90", 
#    "../mesh_object_module.F90", 
#    "../program_parallel_module.F90", 
#    "../mesh_interface_module.F90",
#    "../postprocess_coefficients_module.F90"
#], "src/Interfaces/")
#
#HOFEM_jl.generate_interfaces(["test/resources/example_mod.F90"], "src/Interfaces/")
modules = HOFEM_jl.deserialize_ast(JSON3.read(read("temp.json", String)))
@info modules
