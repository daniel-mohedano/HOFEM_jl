"""
    generate_interfaces(module_files::Vector{<:AbstractString}, interface_path::AbstractString)

Generate the interface code for Fortran modules contained in `module_files` and
store it in the path provided.
"""
function generate_interfaces(module_files::Vector{<:AbstractString}, interface_path::AbstractString)
  if !(isdir(interface_path))
    @warn "Invalid interface path provided `$interface_path`, not a directory."
    return
  end

  for file in module_files
    if !(isfile(file))
      @warn "Fortran module `$file` not found."
      return
    end
    lines = [strip(line) for line in readlines(file)]
    #modinfo = _parse(lines)
    #print(modinfo)
    #build_ast(read(file, String))

    # build_fortran_interface(module_info, interface_path)
    # build_julia_interface(module_info, interface_path)
  end
end

"""
    parse_module(file::AbstractString)::ModuleInfo

Parse the module contained in the Fortran file provided.
"""
#function parse_module(file::AbstractString)::ModuleInfo
#  if !(isfile(file))
#    @warn "Fortran module not found on parsing"
#    return ModuleInfo("", DerivedType[])
#  end
#
#  file_contents = [strip(line) for line in readlines(file)]
#
#  module_info_name = ""
#  module_info_types = DerivedType[]
#
#  for line in file_contents
#    if startswith(line, "MODULE ")
#      module_info_name = split(line, " ")[end]
#    elseif startswith(line, "TYPE ::")
#      new_type = split(line, " ")[end]
#      push!(module_info_types, populate_derived_type_info(file_contents, new_type))
#    end
#  end
#
#  return ModuleInfo(module_info_name, module_info_types)
#end
#
#"""
#    populate_derived_type_info(file_contents::Vector{<:AbstractString}, type_name::AbstractString)::DerivedType
#
#Parse the file lines provided to extract information regarding the Fortran Type provided.
#"""
#function populate_derived_type_info(file_lines::Vector{<:AbstractString}, type_name::AbstractString)::DerivedType
#  derived_type_info = DerivedType(type_name, DerivedTypeMember[], String[])
#  inside_definition = false
#
#  for line in file_lines
#    if startswith(line, "TYPE :: $type_name")
#      inside_definition = true
#    elseif startswith(line, "END TYPE $type_name")
#      inside_definition = false
#    elseif inside_definition
#      member_type = nothing
#      for inter_type in INTEROPERABLE_TYPES
#        if startswith(line, inter_type.name)
#          member_type = inter_type
#        end
#      end
#
#      # If not interoperable or array type
#      if isnothing(member_type) || occursin("ALLOCATABLE", line) || occursin("POINTER", line) || occursin("(", split(line, "::")[2])
#        continue
#      end
#
#      member_name = strip(split(split(line, "::")[2], "=")[1])
#      push!(derived_type_info.members, DerivedTypeMember(member_name, member_type))
#    elseif startswith(line, "TYPE($type_name), TARGET")
#      var_name = strip(split(line, "::")[2])
#      push!(derived_type_info.variables, var_name)
#    end
#  end
#
#  return derived_type_info
#end
#
#"""
#    build_fortran_interface(module_info::ModuleInfo, interface_path::AbstractString)
#
#Build the Fortran interface for the module provided and save it to the given path.
#"""
#function build_fortran_interface(module_info::ModuleInfo, interface_path::AbstractString)
#  interface_mod_name = module_info.name * "_jl_interface"
#  interface_file_name = joinpath(interface_path, interface_mod_name * ".F90")
#
#  # Check if file exists and custom section is present
#  custom_section = find_custom_section(interface_file_name, T_FORTRAN_COMMENT_PREFIX)
#
#  # Check for routines already implemented in the custom section and avoid implementing them again automatically
#  custom_routines = find_fortran_routines_custom_section(custom_section)
#
#  interface_file_contents = t_doc_header_fortran(Dates.format(Dates.now(), "d U Y"), interface_mod_name)
#  module_code = ""
#
#  for derived_type in module_info.types
#    # Getters and setters
#    for member in derived_type.members
#      setter_name = t_setter_name_fortran(derived_type.name, member.name)
#      getter_name = t_getter_name_fortran(derived_type.name, member.name)
#
#      if !(setter_name in custom_routines)
#        if member.type.name != "CHARACTER"
#          setter_code = indent_code(t_setter_fortran(derived_type.name, member.name, member.type.name, member.type.f_type), 1, false)
#        else
#          setter_code = indent_code(t_setter_string_fortran(derived_type.name, member.name), 1, false)
#        end
#
#        module_code *= indent_code(setter_code, 0, true)
#      end
#
#      if !(getter_name in custom_routines)
#        if member.type.name != "CHARACTER"
#          getter_code = indent_code(t_getter_fortran(derived_type.name, member.name, member.type.name, member.type.f_type), 1, false)
#        else
#          getter_code = indent_code(t_getter_string_fortran(derived_type.name, member.name), 1, false)
#        end
#
#        module_code *= indent_code(getter_code, 0, true)
#      end
#    end
#
#    # Module variables
#    for module_var in derived_type.variables
#      getter_name = t_getter_module_var_name(module_var)
#      if !(getter_name in custom_routines)
#        getter_code = indent_code(t_getter_module_var_fortran(module_var, derived_type.name), 1, false)
#        module_code *= indent_code(getter_code, 0, true)
#      end
#    end
#
#    # Custom print function
#    printer_name = t_type_print_name(derived_type.name)
#    if !(printer_name in custom_routines)
#      member_names = String[member.name for member in derived_type.members]
#      print_code = indent_code(t_type_print_fortran(derived_type.name, member_names), 1, false)
#      module_code *= indent_code(print_code, 0, true)
#    end
#  end
#
#  module_code = indent_code(module_code, 1, false)
#  interface_file_contents *= t_module_structure_fortran(interface_mod_name, module_info.name, module_code, custom_section)
#
#  open(interface_file_name, "w") do f
#    write(f, interface_file_contents)
#    @info "Fortran interface written to $interface_file_name"
#  end
#end
#
#"""
#    build_julia_interface(module_info::ModuleInfo, interface_path::AbstractString)
#
#Build the Julia interface for the module provided and save it to the given path.
#"""
#function build_julia_interface(module_info::ModuleInfo, interface_path::AbstractString)
#  interface_mod_name = module_info.name * "_jl_interface"
#  interface_file_name = joinpath(interface_path, interface_mod_name * ".jl")
#
#  # Check if file exists and custom section is present
#  custom_section = find_custom_section(interface_file_name, T_JULIA_COMMENT_PREFIX)
#
#  # Check for routines already implemented in the custom section and avoid implementing them again automatically
#  custom_routines = find_julia_routines_custom_section(custom_section)
#
#  interface_file_contents = t_doc_header_julia(Dates.format(Dates.now(), "d U Y"), interface_mod_name)
#  module_code = ""
#
#  for derived_type in module_info.types
#    # Getters and setters
#    for member in derived_type.members
#      setter_name = t_setter_name_julia(derived_type.name, member.name)
#      getter_name = t_getter_name_julia(derived_type.name, member.name)
#
#      if !(setter_name in custom_routines)
#        if member.type.name != "CHARACTER"
#          setter_code = indent_code(t_setter_julia(derived_type.name, member.name, member.type.jl_type), 0, false)
#        else
#          setter_code = indent_code(t_setter_string_julia(derived_type.name, member.name), 0, false)
#        end
#
#        @debug "new line requested in julia setter $setter_name"
#        module_code *= indent_code(setter_code, 0, true)
#      end
#
#      if !(getter_name in custom_routines)
#        if member.type.name != "CHARACTER"
#          getter_code = indent_code(t_getter_julia(derived_type.name, member.name, member.type.jl_type), 0, false)
#        else
#          getter_code = indent_code(t_getter_string_julia(derived_type.name, member.name), 0, false)
#        end
#
#        module_code *= indent_code(getter_code, 0, true)
#      end
#    end
#
#    # Module variables
#    for module_var in derived_type.variables
#      getter_name = t_getter_module_var_name(module_var)
#      if !(getter_name in custom_routines)
#        getter_code = indent_code(t_getter_module_var_julia(module_var), 0, false)
#        module_code *= indent_code(getter_code, 0, true)
#      end
#    end
#
#    # Custom print function
#    printer_name = t_type_print_name(derived_type.name)
#    if !(printer_name in custom_routines)
#      print_code = indent_code(t_type_print_julia(derived_type.name), 0, false)
#      module_code *= indent_code(print_code, 0, true)
#    end
#  end
#
#  module_code = indent_code(module_code, 0, false)
#  interface_file_contents *= t_module_structure_julia(module_code, custom_section)
#
#  open(interface_file_name, "w") do f
#    write(f, interface_file_contents)
#    @info "Julia interface written to $interface_file_name"
#  end
#end
#
#"""
#    find_custom_section(file_name::AbstractString, delimiter::AbstractString)::String
#Find custom section, if present, in a file and extract its contents.
#"""
#function find_custom_section(file_name::AbstractString, delimiter::AbstractString)::String
#  custom_section_delimiter = [-1, -1]
#  custom_section = ""
#
#  if !isfile(file_name)
#    return "$(delimiter)$(T_CUSTOM_SECTION_MARKER_BEGIN) $(T_CUSTOM_SECTION_INFO)\n\n$(delimiter)$(T_CUSTOM_SECTION_MARKER_END)"
#  end
#
#  lines = readlines(file_name)
#  for (i, line) in enumerate(lines)
#    if startswith(strip(line), delimiter * T_CUSTOM_SECTION_MARKER_BEGIN)
#      custom_section_delimiter[1] = i + 1
#    elseif custom_section_delimiter[1] != -1 && startswith(strip(line), delimiter * T_CUSTOM_SECTION_MARKER_END)
#      custom_section_delimiter[end] = i - 1
#    end
#  end
#
#  if custom_section_delimiter[end] != -1 && custom_section_delimiter[1] <= custom_section_delimiter[end]
#    custom_section = join(lines[custom_section_delimiter[1]:custom_section_delimiter[end]], "\n")
#  end
#
#  custom_section = "$(delimiter)$(T_CUSTOM_SECTION_MARKER_BEGIN) $(T_CUSTOM_SECTION_INFO)\n$(custom_section)\n$(delimiter)$(T_CUSTOM_SECTION_MARKER_END)"
#  return custom_section
#end
#
#"""
#    find_fortran_routines_custom_section(custom_section::AbstractString)::Vector{String}
#Search a custom section for implemented Fortran routines and return their names.
#"""
#function find_fortran_routines_custom_section(custom_section::AbstractString)::Vector{String}
#  custom_routines = String[]
#  for line in split(custom_section, "\n")
#    line = strip(line)
#
#    if startswith(line, "FUNCTION") || startswith(line, "SUBROUTINE")
#      routine_name = strip(split(split(line, " ")[2], "(")[1])
#      push!(custom_routines, routine_name)
#    end
#  end
#
#  return custom_routines
#end
#
#"""
#    find_julia_routines_custom_section(custom_section::AbstractString)::Vector{String} 
#Search a custom section for implemented Julia routines and return their names.
#"""
#function find_julia_routines_custom_section(custom_section::AbstractString)::Vector{String}
#  custom_routines = String[]
#  for line in split(custom_section, "\n")
#    line = strip(line)
#
#    if startswith(line, "function")
#      routine_name = strip(split(split(line, " ")[2], "(")[1])
#      push!(custom_routines, routine_name)
#    end
#  end
#
#  return custom_routines
#end
#
#"""
#    indent_code(code::AbstractString, num_tabs::Int, new_line::Bool)::String
#Indent provided code with a certain number of tabs.
#"""
#function indent_code(code::AbstractString, num_tabs::Int, new_line::Bool)::String
#  lines = split(code, "\n")
#  empty_last = lines[end] == ""
#
#  for i in eachindex(lines)
#    indent = repeat("    ", num_tabs)
#    lines[i] = indent * lines[i]
#  end
#
#  if empty_last
#    new_string = join(lines[1:end-1], "\n") * "\n"
#  else
#    new_string = join(lines, "\n")
#  end
#
#  if new_line
#    return new_string * "\n"
#  else
#    return new_string
#  end
#end

end #module
