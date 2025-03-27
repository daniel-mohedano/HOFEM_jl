include("Types.jl")
using Dates: now, format

@enum Matches begin
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
  TYPE_ATTRIBUTES
  VISIBILITY
end

_REGEXES = Dict(
  # Captures: (1) module name
  MODULE_START => r"MODULE (\w+)",
  # Captures: (1) module name
  MODULE_END => r"END MODULE (\w+)",
  # Captures: (1) type name
  TYPE_START => r"TYPE\s*(?:,\s*(?:(?:ABSTRACT|PUBLIC|PRIVATE|EXTENDS\(\w+\))\s*,?\s*)*)?::\s*(\w+)",
  # Captures: (1) type name
  TYPE_END => r"END TYPE (\w+)",
  # Captures: (1) variable type with optional kind, (2) variable name
  VARIABLE => r"(\w+(?:\([^)]*\))?)\s*(?:,\s*(?:PARAMETER|ALLOCATABLE|POINTER|TARGET|INTENT\((?:IN|OUT|INOUT)\)|DIMENSION\([^)]*\))\s*,?\s*)*::\s*(\w+)(?:\([\w:,]*\))?(?:\s*=\s*[^!]*)?",
  # Captures: (1) derived type name, (2) variable name
  GLOBAL_VAR => r"TYPE\((\w+)\)(?:\s*,\s*(?:TARGET|ALLOCATABLE|POINTER|PUBLIC|PRIVATE))?\s*::\s*(\w+)",
  # Captures: (1) subroutine name, (2) argument list
  SUBROUTINE_START => r"(?:(?:PURE|ELEMENTAL)\s+)?SUBROUTINE\s+(\w+)\s*\(([\w\s,]*)\)",
  # Captures: (1) subroutine name
  SUBROUTINE_END => r"END SUBROUTINE (\w+)",
  # Captures: (1) return type with optional kind, (2) function name, (3) argument list
  FUNCTION_START => r"(?:(?:PURE|ELEMENTAL)\s+)?(\w+(?:\([^)]*\))?)\s+FUNCTION\s+(\w+)\s*\(([\w\s,]*)\)",
  # Captures: (1) function name
  FUNCTION_END => r"END FUNCTION (\w+)",
  # Captures: (1) parent type name if EXTENDS present (otherwise no captures)
  TYPE_ATTRIBUTES => r"(?:ABSTRACT|PUBLIC|PRIVATE|EXTENDS\((\w+)\))",
  # No captures, just matches PUBLIC or PRIVATE
  VISIBILITY => r"(?:PUBLIC|PRIVATE)"
)

_START = r"^\s*"
_END = r"\s*$"

function reg_match(match_type::Matches, line::AbstractString)
  return match(_START * _REGEXES[match_type] * _END, line)
end

# Extracts variable attributes
# Used for derived type members, global vars and procedure args
function var_attributes(line::AbstractString)::FVarAttributes
  is_parameter = occursin(r"PARAMETER", line)
  is_allocatable = occursin(r"ALLOCATABLE", line)
  is_pointer = occursin(r"POINTER", line)

  dimensions = nothing
  # Check for dimensions in DIMENSION attribute
  dim_match = match(r"DIMENSION\(([^)]*)\)", line)
  if dim_match !== nothing
    # Parse dimensions into a vector
    dims = String[]
    for dim in split(dim_match.captures[1], ",")
      push!(dims, strip(dim))
    end
    dimensions = dims
  end
  
  # Check for dimensions after variable name
  if dimensions === nothing
    var_dims = match(r"::\s*\w+\(([\w:,\s]+)\)", line)
    if var_dims !== nothing
      dims = String[]
      for dim in split(var_dims.captures[1], ",")
        push!(dims, strip(dim))
      end
      dimensions = dims
    end
  end

  # Check for intent
  intent = nothing
  intent_match = match(r"INTENT\((IN|OUT|INOUT)\)", line)
  if intent_match !== nothing
    intent = lowercase(intent_match.captures[1])
  end

  return FVarAttributes(is_parameter, is_allocatable, is_pointer, dimensions, intent)
end

# Extracts type attributes
function type_attributes(line::AbstractString)::FDerivedAttributes
  # Check for abstract
  is_abstract = occursin(r"ABSTRACT", line)

  # Check for visibility (default is public)
  is_public = !occursin(r"PRIVATE", line)

  # Check for extends
  extends = nothing
  extends_match = match(r"EXTENDS\((\w+)\)", line)
  if extends_match !== nothing
    extends = extends_match.captures[1]
  end

  return FDerivedAttributes(is_abstract, is_public, extends)
end

# Checks if a procedure is pure or elemental
function procedure_attributes(line::AbstractString)::Tuple{Bool,Bool}
  is_pure = occursin(r"PURE\s+", line)
  is_elemental = occursin(r"ELEMENTAL\s+", line)
  return (is_pure, is_elemental)
end

# Parses visibility
function visibility(line::AbstractString)::Visibility
  return occursin(r"PRIVATE", line) ? Private::Visibility : Public::Visibility
end

# Parses type specifications
function type_spec(spec::AbstractString)::AbstractFType
  # Check if it's a derived type
  m = match(r"TYPE\((\w+)\)", spec)
  if m !== nothing
    return FDerived(m.captures[1], FVar[])
  end

  # Check for kind specification
  m = match(r"(\w+)(?:\(KIND\s*=\s*(\w+|\d+)\))?", spec)
  if m !== nothing
    return FIntrinsic(m.captures[1], m.captures[2])
  end

  # Default case
  return FIntrinsic(spec)
end

function get_date()::String
  return format(now(), "d U Y")
end
