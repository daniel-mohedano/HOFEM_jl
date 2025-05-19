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
  MODULE_START => r"MODULE (?<name>\w+)",
  MODULE_END => r"END MODULE (?<name>\w+)",
  TYPE_START => r"TYPE\s*(?:,\s*(?:(?:ABSTRACT|PUBLIC|PRIVATE|EXTENDS\(\w+\))\s*,?\s*)*)?::\s*(?<name>\w+)",
  TYPE_END => r"END TYPE (?<name>\w+)",
  VARIABLE => r"(?<type>\w+(?:\([^)]*\))?)\s*(?:,\s*(?:PARAMETER|ALLOCATABLE|POINTER|TARGET|INTENT\((?:IN|OUT|INOUT)\)|DIMENSION\([^)]*\))\s*,?\s*)*::\s*(?<name>\w+)(?:\([\w:,]*\))?(?:\s*=\s*[^!]*)?",
  GLOBAL_VAR => r"TYPE\((?<derived_type>\w+)\)(?:\s*,\s*(?:TARGET|ALLOCATABLE|POINTER|PUBLIC|PRIVATE))?\s*::\s*(?<name>\w+)",
  SUBROUTINE_START => r"(?<attribute>PURE|ELEMENTAL)?\s+SUBROUTINE\s+(?<name>\w+)\s*\((?<args>[\w\s,]*)\)(?:\s*BIND(C))?",
  SUBROUTINE_END => r"END SUBROUTINE (?<name>\w+)",
  FUNCTION_START => r"(?:(?<attribute>PURE|ELEMENTAL)\s*)?(?<return_type>\w+(?:\([^)]*\))?)?\s+FUNCTION\s+(?<name>\w+)\s*\((?<args>[\w\s,]*)\)(?:\s*RESULT\((?<return_var>\w+)\))?(?:\s*BIND(C))?",
  FUNCTION_END => r"END FUNCTION (?<name>\w+)",
)

# todo: handle multiple variables in the same line, separated by commas

function Base.match(match_type::MatchType, line::AbstractString)::Union{RegexMatch,Nothing}
  _START = r"^\s*"
  _END = r"\s*$"
  match_str = _START * _REGEXES[match_type] * _END
  return match(match_str, line)
end

# Parses type specifications
function get_type(spec::AbstractString)::AbstractFType
  # Check if it's a derived type
  m = match(r"TYPE\((?<type>\w+)\)", spec)
  if m !== nothing
    return FDerived(m["type"], FVar[])
  end

  # Check for kind specification
  m = match(r"(?<type>\w+)(?:\(KIND\s*=\s*(?<kind>\w+|\d+)\))?", spec)
  if m !== nothing
    return FIntrinsic(m["type"], m["kind"])
  end

  # Default case
  return FIntrinsic(spec)
end

# Extracts variable attributes
# Used for derived type members, global vars and procedure args
function get_var_attributes(line::AbstractString)::FVarAttributes
  is_parameter = occursin(r"PARAMETER", line)
  is_allocatable = occursin(r"ALLOCATABLE", line)
  is_pointer = occursin(r"POINTER", line)

  dimensions = nothing
  # Check for dimensions in DIMENSION attribute
  dim_match = match(r"DIMENSION\((?<dims>[^)]*)\)", line)
  if dim_match !== nothing
    # Parse dimensions into a vector
    dims = String[]
    for dim in split(dim_match["dims"], ",")
      push!(dims, strip(dim))
    end
    dimensions = dims
  end

  # Check for dimensions after variable name
  if dimensions === nothing
    var_dims = match(r"::\s*\w+\((?<dims>[\w:,\s]+)\)", line)
    if var_dims !== nothing
      dims = String[]
      for dim in split(var_dims["dims"], ",")
        push!(dims, strip(dim))
      end
      dimensions = dims
    end
  end

  # Check for intent
  intent = nothing
  intent_match = match(r"INTENT\((?<intent>(IN|OUT|INOUT))\)", line)
  if intent_match !== nothing
    intent = lowercase(intent_match["intent"])
  end

  return FVarAttributes(is_parameter, is_allocatable, is_pointer, dimensions, intent)
end

# Extracts type attributes
function get_type_attributes(line::AbstractString)::FDerivedAttributes
  # Check for abstract
  is_abstract = occursin(r"ABSTRACT", line)

  # Check for visibility (default is public)
  is_public = !occursin(r"PRIVATE", line)

  # Check for extends
  extends = nothing
  extends_match = match(r"EXTENDS\((?<extends>\w+)\)", line)
  if extends_match !== nothing
    extends = extends_match["extends"]
  end

  return FDerivedAttributes(is_abstract, is_public, extends)
end

# Parses visibility
function get_visibility(line::AbstractString)::Visibility
  return occursin(r"PRIVATE", line) ? Private::Visibility : Public::Visibility
end
