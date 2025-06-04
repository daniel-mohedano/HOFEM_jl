@enum MatchType begin
  MODULE_START
  MODULE_END
  TYPE_START
  TYPE_END
  VARIABLE
  GLOBAL_VAR
  SUBROUTINE_START
  SUBROUTINE_END
  FUNCTION_START
  FUNCTION_END
end

const _REGEXES = Dict(
  MODULE_START => r"^\s*MODULE\s+(?<name>\w+)\s*"i,
  MODULE_END => r"^\s*END\s+MODULE\s+(?<name>\w+)\s*"i,
  TYPE_START => r"^\s*TYPE(?<attrs>(\s*,\s*(EXTENDS\(\s*\w+\s*\)|ABSTRACT|PUBLIC|PRIVATE))*)?\s*::\s*(?<name>\w+)\s*"i,
  TYPE_END => r"^\s*END\s+TYPE\s+(?<name>\w+)\s*"i,
  VARIABLE => r"^\s*(?<type>\w+(?:\([^)]*\))?)(?<attrs>(?:\s*,\s*(?:INTENT\((?:IN|OUT|INOUT)\)|DIMENSION\([^)]*\)|PARAMETER|ALLOCATABLE|POINTER|TARGET|VALUE))*)?\s*::\s*(?<names>(?:\w+(?:\([\w:,]*\))?(?:\s*=\s*[^!,]+?)?)(?:\s*,\s*\w+(?:\([\w:,]*\))?(?:\s*=\s*[^!,]+?)?)*)"i,
  GLOBAL_VAR => r"^\s*TYPE\((?<type>\w+)\)(?<attrs>(\s*,\s*(TARGET|ALLOCATABLE|POINTER|PUBLIC|PRIVATE))*)?\s*::\s*(?<name>\w+)\s*"i,
  SUBROUTINE_START => r"^\s*((?<attr>(PURE|ELEMENTAL))\s+)?SUBROUTINE\s+(?<name>\w+)\s*\((?<args>[\w\s,]*)\)(\s*BIND(C))?\s*"i,
  SUBROUTINE_END => r"^\s*END\s+SUBROUTINE(\s+(?<name>\w+))?\s*"i,
  FUNCTION_START => r"^\s*((?<attr>(PURE|ELEMENTAL))\s+)?((?<return_type>\w+(\([^)]*\))?)\s+)?FUNCTION\s+(?<name>\w+)\s*\((?<args>[\w\s,]*)\)(\s*RESULT\((?<return_var>\w+)\))?(\s*BIND(C))?\s*"i,
  FUNCTION_END => r"^\s*END\s+FUNCTION(\s+(?<name>\w+))?\s*"i,
)

function Base.match(match_type::MatchType, line::AbstractString)::Union{RegexMatch,Nothing}
  return match(_REGEXES[match_type], line)
end

function get_type(type::AbstractString)::Union{AbstractType,Nothing}
  if (lowercase(strip(type)) == "procedure")
    # discard procedures that are matched for "VARIABLE" mach
    return nothing
  end

  m = match(r"TYPE\((?<type>\w+)\)"i, type)
  if !isnothing(m)
    return DerivedType(m["type"])
  end

  # Class routines
  m = match(r"CLASS\((?<type>\w+)\)"i, type)
  if !isnothing(m)
    return DerivedType(m["type"])
  end

  m = match(r"(?<type>\w+)(\(((KIND\s*=\s*(?<kind>\w+|\d+))|(LEN\s*=\s*(?<len>\w+|\d+)))\))?"i, type)
  if !isnothing(m)
    return IntrinsicType(m["type"], m["kind"], m["len"])
  end

  # Default case
  return IntrinsicType(type)
end

function get_var_attributes(attrs::AbstractString)::VariableAttrs
  is_parameter = occursin(r"\s*,\s*PARAMETER"i, attrs)
  is_allocatable = occursin(r"\s*,\s*ALLOCATABLE"i, attrs)
  is_pointer = occursin(r"\s*,\s*POINTER"i, attrs)
  is_target = occursin(r"\s*,\s*TARGET"i, attrs)

  dimensions = nothing
  dim_match = match(r"DIMENSION\((?<dims>[^)]*)\)"i, attrs)
  if !isnothing(dim_match)
    # Parse dimensions into a vector
    dims = String[]
    for dim in split(dim_match["dims"], ",")
      push!(dims, strip(dim))
    end
    dimensions = dims
  end

  intent = nothing
  intent_match = match(r"INTENT\((?<intent>(IN|OUT|INOUT))\)"i, attrs)
  if !isnothing(intent_match)
    intent = lowercase(intent_match["intent"])
  end

  is_value = occursin(r"\s*,\s*VALUE"i, attrs)

  return VariableAttrs(is_parameter, is_allocatable, is_pointer, is_target, is_value, dimensions, intent)
end

function get_type_attributes(attributes::AbstractString)::DerivedTypeAttrs
  is_abstract = false
  is_public = true
  extends = nothing

  for attr in split(attributes, ",")
    if isempty(attr)
      continue
    end

    attr = strip(attr)
    if lowercase(attr) == "abstract"
      is_abstract = true
    elseif lowercase(attr) == "private"
      is_public = false
    else
      match_obj = match(r"EXTENDS\((?<type>\w+)\)"i, attr)
      if !isnothing(match_obj)
        extends = match_obj["type"]
      end
    end
  end

  return DerivedTypeAttrs(is_abstract, is_public, extends)
end

function get_visibility(attrs::AbstractString)::Visibility
  return occursin(r"\s*,\s*PRIVATE"i, attrs) ? Private::Visibility : Public::Visibility
end
