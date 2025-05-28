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

parser = HOFEM_jl.RegexParserImpl()
modules = HOFEM_jl.Parsing.parse(parser, ["../formulation_terms_module.F90"])
#for der in modules[1].types
#  if der.name == "FormulationTerms"
#    show(stdout, MIME"text/plain"(), der)
#  end
#end
HOFEM_jl.generate_interfaces(modules, ".")
