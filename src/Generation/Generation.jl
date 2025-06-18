module Generation

using Dates
import ..Parsing: AbstractModule, DerivedType, IntrinsicType, ModuleVariable, Variable, Procedure, fortran_type, julia_type, is_interoperable, isstring, MatchType, SUBROUTINE_START, FUNCTION_START

export generate_interfaces

include("Templates.jl")

# todo: clean function returns

"""
    generate_interfaces(modules::Vector{Module}, output_path::AbstractString)
"""
function generate_interfaces(modules::Vector{<:AbstractModule}, output_path::AbstractString)
  if !(isdir(output_path))
    @warn "Invalid interface path provided `$output_path`, not a directory."
    return
  end

  for mod in modules
    build_fortran_interface(mod, output_path)
    build_julia_interface(mod, output_path)
  end
end

"""
    build_fortran_interface(mod::Module, output_path::AbstractString)

Build the Fortran interface for the module provided and save it to the given path.
"""
function build_fortran_interface(mod::AbstractModule, output_path::AbstractString)
  interface_mod_name = mod.name * "_jl_interface"
  interface_file_name = joinpath(output_path, interface_mod_name * ".F90")

  # Check if file exists and custom section is present
  custom_section = find_custom_section(interface_file_name, FORTRAN)

  # Check for routines already implemented in the custom section and avoid implementing them again automatically
  custom_routines = find_fortran_routines_custom_section(custom_section)

  interface_file_contents = t_doc_header_fortran(Dates.format(Dates.now(), "d U Y"), interface_mod_name)
  module_code = ""

  # Derived type getters and setters
  for derived_type in mod.types
    for member in derived_type.members
      module_code *= build_member_access(derived_type.name, member, custom_routines, FORTRAN)
    end
    # Custom print function
    module_code *= build_type_print(derived_type, custom_routines, FORTRAN)
    # Type factory
    module_code *= t_factory(derived_type.name, FORTRAN) * "\n"
  end

  # Module variables
  for module_var in mod.variables
    module_code *= build_module_var_access(module_var, custom_routines, FORTRAN)
  end
  #
  # Routines
  for proc in mod.procedures
    module_code *= t_call_routine(proc, FORTRAN) * "\n"
  end

  module_code = indent_code(module_code, 2, false)

  interface_file_contents *= t_module_structure_fortran(interface_mod_name, mod.name, module_code, custom_section)

  open(interface_file_name, "w") do f
    write(f, interface_file_contents)
    @info "Fortran interface written to $interface_file_name"
  end
end

"""
    build_julia_interface(mod::Module, output_path::AbstractString)

Build the Julia interface for the module provided and save it to the given path.
"""
function build_julia_interface(mod::AbstractModule, output_path::AbstractString)
  interface_mod_name = mod.name * "_jl_interface"
  interface_file_name = joinpath(output_path, interface_mod_name * ".jl")

  # Check if file exists and custom section is present
  custom_section = find_custom_section(interface_file_name, JULIA)

  # Check for routines already implemented in the custom section and avoid implementing them again automatically
  custom_routines = find_julia_routines_custom_section(custom_section)

  interface_file_contents = t_doc_header_julia(Dates.format(Dates.now(), "d U Y"), interface_mod_name)
  module_code = ""

  # Derived type getters and setters
  for derived_type in mod.types
    for member in derived_type.members
      module_code *= build_member_access(derived_type.name, member, custom_routines, JULIA)
    end
    # Custom print function
    module_code *= build_type_print(derived_type, custom_routines, JULIA)
    # Type factory
    module_code *= t_factory(derived_type.name, JULIA) * "\n"
  end

  # Module variables
  for module_var in mod.variables
    module_code *= build_module_var_access(module_var, custom_routines, JULIA)
  end

  # Routines
  for proc in mod.procedures
    module_code *= t_call_routine(proc, JULIA) * "\n"
  end

  module_code = indent_code(module_code, 0, false)
  interface_file_contents *= t_module_structure_julia(module_code, custom_section)

  open(interface_file_name, "w") do f
    write(f, interface_file_contents)
    @info "Julia interface written to $interface_file_name"
  end
end

function build_member_access(type_name::AbstractString, member::Variable, custom_routines::Vector{<:AbstractString}, lang::Lang)::AbstractString
  # todo: remove this check and improve interface generation
  if member.type isa DerivedType || !isnothing(member.attributes.dimensions) || !is_interoperable(member)
    return ""
  end

  interoperable_type = lang isa Fortran ? fortran_type : julia_type

  code = ""
  setter_name = t_setter_name(type_name, member.name, lang)
  getter_name = t_getter_name(type_name, member.name, lang)

  if !(setter_name in custom_routines)
    code *= t_setter(type_name, member.name, member.type, interoperable_type(member.type), lang)
    code *= "\n"
  end

  if !(getter_name in custom_routines)
    code *= t_getter(type_name, member.name, member.type, interoperable_type(member.type), lang)
    code *= "\n"
  end

  return code
end

function build_module_var_access(module_var::ModuleVariable, custom_routines::Vector{<:AbstractString}, lang::Lang)::AbstractString
  if !module_var.var.attributes.is_target
    return ""
  end

  code = ""
  getter_name = t_getter_module_var_name(module_var.var.name)
  if !(getter_name in custom_routines)
    code *= t_getter_module_var(module_var.var.name, module_var.var.type.name, lang)
    code *= "\n"
  end

  return code
end

function build_type_print(type::DerivedType, custom_routines::Vector{<:AbstractString}, lang::Lang)::AbstractString
  code = ""
  printer_name = t_type_print_name(type.name)
  if !(printer_name in custom_routines)
    member_names = String[member.name for member in type.members]
    code *= t_type_print(type.name, member_names, lang)
    code *= "\n"
  end

  return code
end

function needs_wrapper(p::Procedure)::Bool
  if !isnothing(p.ret) && !is_interoperable(p.ret)
    return true
  end

  for arg in p.args
    if !is_interoperable(arg)
      return true
    end
  end

  return false
end

"""
    find_custom_section(file_name::AbstractString, delimiter::AbstractString)::String
Find custom section, if present, in a file and extract its contents.
"""
function find_custom_section(file_name::AbstractString, lang::Lang)::String
  delimiter = lang isa Fortran ? T_FORTRAN_COMMENT_PREFIX : T_JULIA_COMMENT_PREFIX
  custom_section_delimiter = [-1, -1]
  custom_section = ""

  if !isfile(file_name)
    return "$(delimiter)$(T_CUSTOM_SECTION_MARKER_BEGIN) $(T_CUSTOM_SECTION_INFO)\n\n$(delimiter)$(T_CUSTOM_SECTION_MARKER_END)"
  end

  lines = readlines(file_name)
  for (i, line) in enumerate(lines)
    if startswith(strip(line), delimiter * T_CUSTOM_SECTION_MARKER_BEGIN)
      custom_section_delimiter[1] = i + 1
    elseif custom_section_delimiter[1] != -1 && startswith(strip(line), delimiter * T_CUSTOM_SECTION_MARKER_END)
      custom_section_delimiter[end] = i - 1
    end
  end

  if custom_section_delimiter[end] != -1 && custom_section_delimiter[1] <= custom_section_delimiter[end]
    custom_section = join(lines[custom_section_delimiter[1]:custom_section_delimiter[end]], "\n")
  end

  custom_section = "$(delimiter)$(T_CUSTOM_SECTION_MARKER_BEGIN) $(T_CUSTOM_SECTION_INFO)\n$(custom_section)\n$(delimiter)$(T_CUSTOM_SECTION_MARKER_END)"
  return custom_section
end

"""
    find_fortran_routines_custom_section(custom_section::AbstractString)::Vector{String}
Search a custom section for implemented Fortran routines and return their names.
"""
function find_fortran_routines_custom_section(custom_section::AbstractString)::Vector{String}
  custom_routines = String[]
  for line in split(custom_section, "\n")
    line = strip(line)

    m = match(SUBROUTINE_START::MatchType, line)
    if !isnothing(m)
      push!(custom_routines, m["name"])
      continue
    end

    m = match(FUNCTION_START::MatchType, line)
    if !isnothing(m)
      push!(custom_routines, m["name"])
      continue
    end

  end

  return custom_routines
end

"""
    find_julia_routines_custom_section(custom_section::AbstractString)::Vector{String} 
Search a custom section for implemented Julia routines and return their names.
"""
function find_julia_routines_custom_section(custom_section::AbstractString)::Vector{String}
  custom_routines = String[]
  for line in split(custom_section, "\n")
    line = strip(line)
    if startswith(line, "function")
      routine_name = strip(split(split(line, " ")[2], "(")[1])
      push!(custom_routines, routine_name)
    end
  end

  return custom_routines
end

"""
    indent_code(code::AbstractString, num_tabs::Int, new_line::Bool)::String
Indent provided code with a certain number of tabs.
"""
function indent_code(code::AbstractString, num_tabs::Int, new_line::Bool)::String
  lines = split(code, "\n")
  empty_last = lines[end] == ""

  for i in eachindex(lines)
    indent = repeat("\t", num_tabs)
    lines[i] = indent * lines[i]
  end

  if empty_last
    new_string = join(lines[1:end-1], "\n") * "\n"
  else
    new_string = join(lines, "\n")
  end

  if new_line
    return new_string * "\n"
  else
    return new_string
  end
end
end # module
