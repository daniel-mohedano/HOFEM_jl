include("src/HOFEM_jl.jl")
using .HOFEM_jl

parser = HOFEM_jl.RegexParserImpl()

modules = HOFEM_jl.Parsing.parse(parser, [
  "../program_variables_module.F90",
  "../formulation_terms_module.F90",
  "../mesh_object_module.F90",
  "../program_parallel_module.F90",
  "../mesh_interface_module.F90",
  "../postprocess_coefficients_module.F90"
])
HOFEM_jl.generate_interfaces(modules, "src/interfaces/")
