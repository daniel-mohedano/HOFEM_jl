include("Types.jl")
include("Utils.jl")

struct RegexParserImpl <: Parser
end

function parse(p::RegexParserImpl, module_files::Vector{<:AbstractString})::Vector{AbstractModule}
  modules = AbstractModule[]

  for file in module_files
    if !(isfile(file))
      @warn "Fortran module `$file` not found."
      continue
    end

    lines = [strip(line) for line in readlines(file)]
    modinfo = _parse(lines)
    if modinfo !== Nothing
      push!(modules, modinfo)
    end
  end

  return modules
end

# Parse a file
function _parse(lines::Vector{<:AbstractString})::Union{FModule,Nothing}
  while length(lines) > 0
    line = popfirst!(lines)

    m = match(MODULE_START::MatchType, line)
    if m !== nothing
      return _parse_module(m["name"], lines)
    end
  end

  return Nothing
end

# Parse a module, which can contain:
# - Derived type definitions
# - Global variables
# - Procedures (subroutines and functions)
function _parse_module(name::AbstractString, lines::Vector{<:AbstractString})::Union{FModule,Nothing}
  types = FDerived[]
  variables = FModuleVar[]
  procedures = FProcedure[]

  while length(lines) > 0
    line = popfirst!(lines)

    m = match(MODULE_END::MatchType, line)
    if m !== nothing && m["name"] == name
      return FModule(name, types, variables, procedures)
    end

    m = match(TYPE_START::MatchType, line)
    if m !== nothing
      parsed_type = _parse_type(m["name"], line, lines)
      if parsed_type !== nothing
        push!(types, parsed_type)
      end
      continue
    end

    m = match(GLOBAL_VAR::MatchType, line)
    if m !== nothing
      var = FVar(m["name"], FDerived(m["derived_type"], FVar[]), get_var_attributes(line))
      push!(variables, FModuleVar(var, get_visibility(line)))
      continue
    end

    m = match(SUBROUTINE_START::MatchType, line)
    if m !== nothing
      parsed_proc = _parse_procedure(m["name"], m["args"], nothing, nothing, m["attribute"], line, lines)
      if parsed_proc !== nothing
        push!(procedures, parsed_proc)
      end
      continue
    end

    m = match(FUNCTION_START::MatchType, line)
    if m !== nothing
      parsed_proc = _parse_procedure(m["name"], m["args"], m["return_type"], m["return_var"], m["attribute"], line, lines)
      if parsed_proc !== nothing
        push!(procedures, parsed_proc)
      end
      continue
    end
  end

  return Nothing
end

# Parse a derived type definition and extract:
# - Members
# - Attributes
function _parse_type(name::AbstractString, type_definition_line::AbstractString, lines::Vector{<:AbstractString})::Union{FDerived,Nothing}
  members = FVar[]
  attributes = get_type_attributes(type_definition_line)

  while length(lines) > 0
    line = popfirst!(lines)
    m = match(TYPE_END::MatchType, line)
    if m !== nothing && m["name"] == name
      return FDerived(name, members, attributes)
    end

    m = match(VARIABLE::MatchType, line)
    if m !== nothing
      var_type = get_type(m["type"])
      var_name = m["name"]
      var_attrs = get_var_attributes(line)
      push!(members, FVar(var_name, var_type, var_attrs))
    end
  end

  return Nothing
end

# Parse a procedure (subroutine or function) and extract:
# - Attributes
# - Arguments
function _parse_procedure(name::AbstractString, args_str::AbstractString, return_type_str::Union{AbstractString,Nothing}, return_var::Union{AbstractString,Nothing}, attribute::Union{AbstractString,Nothing}, proc_line::AbstractString, lines::Vector{<:AbstractString})::Union{FProcedure,Nothing}
  return_type = nothing

  # Parse procedure attributes
  is_pure = attribute == "PURE"
  is_elemental = attribute == "ELEMENTAL"
  vis = get_visibility(proc_line)

  # Parse arguments names
  args = FVar[]
  args_names = String[]
  if !isempty(args_str)
    for arg in split(args_str, ",")
      arg = strip(arg)
      if !isempty(arg)
        push!(args_names, arg)
      end
    end
  end

  # Look for argument declarations and end of procedure
  end_pattern = occursin(r"FUNCTION", proc_line) ? FUNCTION_END::MatchType : SUBROUTINE_END::MatchType

  while length(lines) > 0
    line = popfirst!(lines)

    # Check for procedure end
    m = match(end_pattern, line)
    if m !== nothing && m["name"] == name
      return FProcedure(name, args, return_type, is_pure, is_elemental, vis)
    end

    # Look for argument declarations to update their types and attributes
    for arg in copy(args_names)
      if occursin(arg, line)
        m = match(VARIABLE::MatchType, line)
        if m !== nothing
          var_type = get_type(m["type"])
          var_attrs = get_var_attributes(line)
          # Update the argument's type and attributes
          push!(args, FVar(arg, var_type, var_attrs))
          # Remove the found argument from the list
          filter!(x -> x != arg, args_names)
        end
      end
    end

    if return_var !== nothing && occursin(return_var, line)
      m = match(VARIABLE::MatchType, line)
      if m !== nothing && m["name"] == return_var
        return_type = get_type(m["type"])
      end
    end
  end


  return Nothing
end
