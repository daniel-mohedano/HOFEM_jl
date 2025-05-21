include("Types.jl")
include("Utils.jl")

struct RegexParserImpl <: AbstractParser
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
    if modinfo !== nothing
      push!(modules, modinfo)
    end
  end

  return modules
end

# Parse a file
function _parse(lines::Vector{<:AbstractString})::Union{Module,Nothing}
  while length(lines) > 0
    line = popfirst!(lines)

    m = match(MODULE_START::MatchType, line)
    if m !== nothing
      return _parse_module(m, lines)
    end
  end

  return nothing
end

# Parse a module, which can contain:
# - Derived type definitions
# - Global variables
# - Procedures (subroutines and functions)
function _parse_module(original_match::RegexMatch, lines::Vector{<:AbstractString})::Module
  name = original_match["name"]

  types = DerivedType[]
  variables = ModuleVariable[]
  procedures = Procedure[]

  while length(lines) > 0
    line = popfirst!(lines)

    m = match(MODULE_END::MatchType, line)
    if m !== nothing && m["name"] == name
      return Module(name, types, variables, procedures)
    end

    m = match(TYPE_START::MatchType, line)
    if m !== nothing
      parsed_type = _parse_derived_type(m, lines)
      if parsed_type !== nothing
        push!(types, parsed_type)
      end
      continue
    end

    m = match(GLOBAL_VAR::MatchType, line)
    if m !== nothing
      var = Variable(m["name"], DerivedType(m["type"]), get_var_attributes(m["attrs"]))
      push!(variables, ModuleVariable(var, get_visibility(m["attrs"])))
      continue
    end

    m = match(SUBROUTINE_START::MatchType, line)
    if m !== nothing
      parsed_proc = _parse_procedure(m, false, lines)
      if parsed_proc !== nothing
        push!(procedures, parsed_proc)
      end
      continue
    end

    m = match(FUNCTION_START::MatchType, line)
    if m !== nothing
      parsed_proc = _parse_procedure(m, true, lines)
      if parsed_proc !== nothing
        push!(procedures, parsed_proc)
      end
      continue
    end
  end

  return Module(name, types, variables, procedures)
end

# Parse a derived type definition and extract:
# - Members
# - Attributes
function _parse_derived_type(original_match::RegexMatch, lines::Vector{<:AbstractString})::DerivedType
  name = original_match["name"]

  members = Variable[]
  attributes = get_type_attributes(original_match["attrs"])

  while length(lines) > 0
    line = popfirst!(lines)
    m = match(TYPE_END::MatchType, line)
    if m !== nothing && m["name"] == name
      return DerivedType(name, members, attributes)
    end

    m = match(VARIABLE::MatchType, line)
    if m !== nothing
      variables = _parse_variables(m)
      append!(members, variables)
    end
  end

  return DerivedType(name, members, attributes)
end

# Parse a procedure (subroutine or function) and extract:
# - Attributes
# - Arguments
function _parse_procedure(original_match::RegexMatch, is_function::Bool, lines::Vector{<:AbstractString})::Procedure
  name = original_match["name"]

  return_type = haskey(original_match, "return_type") && !isnothing(original_match["return_type"]) ? get_type(original_match["return_type"]) : nothing
  return_var = haskey(original_match, "return_var") ? original_match["return_var"] : nothing

  attrs = original_match["attr"]
  is_pure = false
  is_elemental = false
  vis = Public::Visibility
  if attrs !== nothing
    if occursin(r"PURE"i, attrs)
      is_pure = true
    end
    if occursin(r"ELEMENTAL"i, attrs)
      is_elemental = true
    end
  end

  args_str = original_match["args"]
  args = Variable[]
  arg_names = String[]
  if !isempty(args_str)
    for arg in split(args_str, ",")
      arg = strip(arg)
      if !isempty(arg)
        push!(arg_names, arg)
      end
    end
  end

  end_pattern = is_function ? FUNCTION_END::MatchType : SUBROUTINE_END::MatchType

  while length(lines) > 0
    line = popfirst!(lines)

    m = match(end_pattern, line)
    if m !== nothing && m["name"] == name
      return Procedure(name, args, return_type, is_pure, is_elemental, vis)
    end

    # Look for argument declarations to update their types and attributes
    m = match(VARIABLE::MatchType, line)
    if isnothing(m)
      continue
    end

    variables = _parse_variables(m)
    for variable in variables
      if variable.name in arg_names
        push!(args, variable)
      elseif is_function && isnothing(return_type) && (variable.name == return_var || variable.name == name)
        return_type = variable.type
      end
    end
  end

  return Procedure(name, args, return_type, is_pure, is_elemental, vis)
end

function _parse_variables(original_match::RegexMatch)::Vector{Variable}
  variables = Variable[]

  shared_type = get_type(original_match["type"])
  shared_attrs = get_var_attributes(original_match["attrs"])

  for var in split(original_match["names"], r"\s*,\s*(?![^()]*\))")
    m = match(r"^\s*(?<name>\w+)(\((?<dims>[^)]*)\))?\s*(?:=.*)?$", strip(var))
    if m !== nothing
      name = m["name"]
      dim_match = m["dims"]

      if dim_match !== nothing
        dims = [strip(dim) for dim in split(dim_match, ",")]
        new_attrs = VariableAttrs(
          shared_attrs.is_parameter,
          shared_attrs.is_allocatable,
          shared_attrs.is_pointer,
          dims,
          shared_attrs.intent)
        push!(variables, Variable(name, shared_type, new_attrs))
      else
        push!(variables, Variable(name, shared_type, shared_attrs))
      end
    end
  end

  return variables
end
