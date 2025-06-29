module Generation

using Dates
import ..Parsing: AbstractModule, DerivedType, IntrinsicType, ModuleVariable, Variable, Procedure, fortran_type, julia_type, isstring, needs_wrapper, MatchType, SUBROUTINE_START, FUNCTION_START

export generate_interfaces

include("Templates.jl")

"""
Generate Fortran and Julia interfaces for the provided parsed modules.
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
Build Fortran interface for the provided module.
"""
function build_fortran_interface(mod::AbstractModule, output_path::AbstractString)
  interface_mod_name = mod.name * "_jl_interface"
  interface_file_name = joinpath(output_path, interface_mod_name * ".F90")

  # Check if file exists, custom section is present and for already implemented routines
  custom_section = find_custom_section(interface_file_name, FORTRAN)
  custom_routines = find_fortran_routines_custom_section(custom_section)

  interface_file_contents = t_doc_header_fortran(Dates.format(Dates.now(), "d U Y"), interface_mod_name)
  module_code = ""

  # Derived type getters and setters
  for derived_type in mod.types
    module_code *= t_factory(derived_type.name, FORTRAN) * "\n"

    for member in derived_type.members
      module_code *= build_member_access(derived_type.name, member, custom_routines, FORTRAN)
    end

    module_code *= build_type_print(derived_type, custom_routines, FORTRAN)
  end

  # Module variables
  for module_var in mod.variables
    module_code *= build_module_var_access(module_var, custom_routines, FORTRAN)
  end

  # Procedures (functions and subroutines)
  for procedure in mod.procedures
    module_code *= build_procedure_interface(procedure, mod.name, custom_routines, FORTRAN)
  end

  module_code = indent_code(module_code, 2, false)
  interface_file_contents *= t_module_structure_fortran(interface_mod_name, mod.name, module_code, custom_section)

  open(interface_file_name, "w") do f
    write(f, interface_file_contents)
    @info "Fortran interface written to $interface_file_name"
  end
end

"""
Build Julia interface for the provided module.
"""
function build_julia_interface(mod::AbstractModule, output_path::AbstractString)
  interface_mod_name = mod.name * "_jl_interface"
  interface_file_name = joinpath(output_path, interface_mod_name * ".jl")

  # Check if file exists, custom section is present and for already implemented routines
  custom_section = find_custom_section(interface_file_name, JULIA)
  custom_routines = find_julia_routines_custom_section(custom_section)

  interface_file_contents = t_doc_header_julia(Dates.format(Dates.now(), "d U Y"), interface_mod_name)
  module_code = ""

  # Derived type getters and setters
  for derived_type in mod.types
    module_code *= t_factory(derived_type.name, JULIA) * "\n"

    for member in derived_type.members
      module_code *= build_member_access(derived_type.name, member, custom_routines, JULIA)
    end

    module_code *= build_type_print(derived_type, custom_routines, JULIA)
  end

  # Module variables
  for module_var in mod.variables
    module_code *= build_module_var_access(module_var, custom_routines, JULIA)
  end

  # Procedures (functions and subroutines)
  for procedure in mod.procedures
    module_code *= build_procedure_interface(procedure, mod.name, custom_routines, JULIA)
  end

  module_code = indent_code(module_code, 0, false)
  interface_file_contents *= t_module_structure_julia(module_code, custom_section)

  open(interface_file_name, "w") do f
    write(f, interface_file_contents)
    @info "Julia interface written to $interface_file_name"
  end
end

"""
Returns code (for the `lang` language) of getter and setter for `member`.
"""
function build_member_access(type_name::AbstractString, member::Variable, custom_routines::Vector{<:AbstractString}, lang::Lang)::String
  # TODO: remove this check and improve interface generation
  if member.type isa DerivedType || !isnothing(member.attributes.dimensions)
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

"""
Returns code (for the `lang` language) of the interface function to access `module_var`.
"""
function build_module_var_access(module_var::ModuleVariable, custom_routines::Vector{<:AbstractString}, lang::Lang)::String
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

"""
Returns code (for the `lang` language) of the custom print routine for `type`.
"""
function build_type_print(type::DerivedType, custom_routines::Vector{<:AbstractString}, lang::Lang)::String
  code = ""
  printer_name = t_type_print_name(type.name)
  if !(printer_name in custom_routines)
    member_names = String[member.name for member in type.members]
    code *= t_type_print(type.name, member_names, lang)
    code *= "\n"
  end

  return code
end

"""
Returns Fortran interface code for `procedure`. Will generate any additional code 
needed by it, like wrappers and structures.
"""
function build_procedure_interface(procedure::Procedure, module_name::AbstractString, custom_routines::Vector{<:AbstractString}, lang::Fortran)::String
  if procedure.name in custom_routines
    return ""
  end

  code = ""
  if needs_wrapper(procedure)
    wrapper_name = t_procedure_wrapper_name(procedure.name)
    if !(wrapper_name in custom_routines)
      code *= build_fortran_wrapper(procedure)
      code *= "\n"
    end
  end

  return code
end

"""
Builds Fortran wrapper logic for `procedure` to be called from Julia.
"""
function build_fortran_wrapper(procedure::Procedure)::AbstractString
  args = Any[]
  pointer_conversions = String[]

  for arg in procedure.args
    if arg.type isa IntrinsicType
      fortran_decl = ""
      call_name = arg.name

      # Handle arrays
      if !isnothing(arg.attributes.dimensions)
        fortran_decl = "TYPE(C_PTR), VALUE :: $(arg.name)"
        ptr_var = "$(arg.name)_f"
        call_name = ptr_var

        # Generate pointer conversion code
        if !isnothing(arg.attributes.intent) && arg.attributes.intent in ["out", "inout"]
          push!(pointer_conversions, "$(fortran_type(arg.type)), POINTER :: $(ptr_var)(:)")
          push!(pointer_conversions, "CALL c_f_pointer($(arg.name), $(ptr_var), [size])")
        else
          push!(pointer_conversions, "$(fortran_type(arg.type)), POINTER :: $(ptr_var)(:)")
          push!(pointer_conversions, "CALL c_f_pointer($(arg.name), $(ptr_var), [size])")
        end
        # Handle scalars with intent
      elseif !isnothing(arg.attributes.intent) && arg.attributes.intent in ["out", "inout"]
        fortran_decl = "$(fortran_type(arg.type)), INTENT(INOUT) :: $(arg.name)"
        call_name = arg.name
        # Handle regular scalars
      else
        fortran_decl = "$(fortran_type(arg.type)), VALUE :: $(arg.name)"
        call_name = arg.name
      end

      push!(args, (name=arg.name, fortran_decl=fortran_decl, call_name=call_name))
    elseif arg.type isa DerivedType
      # Handle derived type arguments
      fortran_decl = "TYPE(C_PTR), VALUE :: $(arg.name)"
      ptr_var = "$(arg.name)_f"
      call_name = ptr_var

      # Generate pointer conversion for derived types
      if !isnothing(arg.attributes.intent) && arg.attributes.intent in ["out", "inout"]
        push!(pointer_conversions, "TYPE($(arg.type.name)), POINTER :: $(ptr_var)")
        push!(pointer_conversions, "CALL c_f_pointer($(arg.name), $(ptr_var))")
      else
        push!(pointer_conversions, "TYPE($(arg.type.name)), POINTER :: $(ptr_var)")
        push!(pointer_conversions, "CALL c_f_pointer($(arg.name), $(ptr_var))")
      end

      push!(args, (name=arg.name, fortran_decl=fortran_decl, call_name=call_name))
    end
  end

  ret_type = nothing
  if procedure.ret !== nothing
    if procedure.ret.type isa IntrinsicType
      ret_type = fortran_type(procedure.ret.type)
    elseif procedure.ret.type isa DerivedType
      ret_type = "TYPE(C_PTR)"
    end
  end

  return t_procedure_fortran_wrapper(procedure.name, args, ret_type, pointer_conversions)
end

"""
Returns Julia interface code for `procedure`. Will generate any additional code 
needed by it, like wrappers and structures.
"""
function build_procedure_interface(procedure::Procedure, module_name::AbstractString, custom_routines::Vector{<:AbstractString}, lang::Julia)::String
  if procedure.name in custom_routines
    return ""
  end

  args = []
  for arg in procedure.args
    if arg.type isa IntrinsicType
      arg_type = ""

      # Handle arrays
      if !isnothing(arg.attributes.dimensions)
        # Special case for character arrays (strings)
        if lowercase(arg.type.name) == "character"
          arg_type = julia_type(arg.type)  # Already Ptr{Cchar}
        else
          arg_type = "Ptr{$(julia_type(arg.type))}"
        end
      elseif !isnothing(arg.attributes.intent) && arg.attributes.intent in ["out", "inout"]
        arg_type = "Ref{$(julia_type(arg.type))}"
      else
        if procedure.is_bind_c && arg.attributes.is_value
          arg_type = julia_type(arg.type)
        else
          arg_type = "Ref{$(julia_type(arg.type))}"
        end
      end
      push!(args, (name=arg.name, arg_type=arg_type, call_str="$(arg.name)::$arg_type"))
    elseif arg.type isa DerivedType
      push!(args, (name=arg.name, arg_type=arg.type.name, call_str="$(arg.name).handle"))
    end
  end

  ret_type = nothing
  if procedure.ret !== nothing
    if procedure.ret.type isa IntrinsicType
      # Check if return type is an array
      if !isnothing(procedure.ret.attributes.dimensions)
        ret_type = "Ptr{$(julia_type(procedure.ret.type))}"
      else
        ret_type = julia_type(procedure.ret.type)
      end
    elseif procedure.ret.type isa DerivedType && procedure.ret.type.name == "c_ptr"
      ret_type = "Ptr{Cvoid}"
    end
    # TODO: handle derived type returns
  end

  return t_procedure_julia_call(procedure.name, module_name, !needs_wrapper(procedure), procedure.is_bind_c, args, ret_type) * "\n"
end

"""
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
Search a custom section for implemented Julia routines and return their names.
"""
function find_julia_routines_custom_section(custom_section::AbstractString)::Vector{String}
  custom_routines = String[]
  for line in split(custom_section, "\n")
    line = strip(line)
    if startswith(line, "function")
      routine_name = rstrip(strip(split(split(line, " ")[2], "(")[1]), ['!'])
      push!(custom_routines, routine_name)
    end
  end

  return custom_routines
end

"""
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
