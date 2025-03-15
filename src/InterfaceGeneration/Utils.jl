module Utils

include("Types.jl")
using .Types

using Dates: now, format

@enum Matches begin
  MODULE_START
  MODULE_END
  TYPE_START
  TYPE_END
  MEMBER
  GLOBAL_VAR
  SUBROUTINE_START
  SUBROUTINE_END
  FUNCTION_START
  FUNCTION_END
  VARIABLE_ATTRIBUTES
  TYPE_ATTRIBUTES
  VISIBILITY
end
export Matches

_REGEXES = Dict(
  MODULE_START => r"MODULE (\w+)",
  MODULE_END => r"END MODULE (\w+)",
  TYPE_START => r"TYPE\s*(?:,\s*(?:(?:ABSTRACT|PUBLIC|PRIVATE|EXTENDS\(\w+\))\s*,?\s*)*)?::\s*(\w+)",
  TYPE_END => r"END TYPE (\w+)",
  MEMBER => r"(\w+(?:\([^)]*\))?)\s*::\s*(\w+)(?:\([\w:,]*\))?(?:\s*=\s*[^!]*)?",
  GLOBAL_VAR => r"TYPE\((\w+)\)(?:\s*,\s*(?:TARGET|ALLOCATABLE|POINTER|PUBLIC|PRIVATE))?\s*::\s*(\w+)",
  SUBROUTINE_START => r"(?:(?:PURE|ELEMENTAL)\s+)?SUBROUTINE\s+(\w+)\s*\(([\w\s,]*)\)",
  SUBROUTINE_END => r"END SUBROUTINE (\w+)",
  FUNCTION_START => r"(?:(?:PURE|ELEMENTAL)\s+)?(\w+(?:\([^)]*\))?)\s+FUNCTION\s+(\w+)\s*\(([\w\s,]*)\)",
  FUNCTION_END => r"END FUNCTION (\w+)",
  VARIABLE_ATTRIBUTES => r"(?:PARAMETER|ALLOCATABLE|POINTER|TARGET|INTENT\((?:IN|OUT|INOUT)\)|DIMENSION\([^)]*\))",
  TYPE_ATTRIBUTES => r"(?:ABSTRACT|PUBLIC|PRIVATE|EXTENDS\((\w+)\))",
  VISIBILITY => r"(?:PUBLIC|PRIVATE)"
)

_START = r"^\s*"
_END = r"\s*$"

function reg_match(match_type::Matches, line::AbstractString)
  return match(_START * _REGEXES[match_type] * _END, line)
end
export reg_match

# Helper function to extract variable attributes from a line
# Used for derived type members, global vars and procedure args
function var_attributes(line::AbstractString)::FVarAttributes
  is_parameter = occursin(r"PARAMETER", line)
  is_allocatable = occursin(r"ALLOCATABLE", line)
  is_pointer = occursin(r"POINTER", line)

  # Check for dimensions
  dimensions = nothing
  dim_match = match(r"DIMENSION\(([^)]*)\)", line)
  if dim_match !== nothing
    # Parse dimensions into a vector of Int or String
    dims = String[]
    for dim in split(dim_match.captures[1], ",")
      push!(dims, strip(dim))
    end
    dimensions = dims
  end

  # Check for intent
  intent = nothing
  intent_match = match(r"INTENT\((IN|OUT|INOUT)\)", line)
  if intent_match !== nothing
    intent = lowercase(intent_match.captures[1])
  end

  return FVarAttributes(is_parameter, is_allocatable, is_pointer, dimensions, intent)
end
export var_attributes

# Helper function to extract type attributes from a line
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
export type_attributes

# Helper function to check if a procedure is pure or elemental
function procedure_attributes(line::AbstractString)
  is_pure = occursin(r"PURE\s+", line)
  is_elemental = occursin(r"ELEMENTAL\s+", line)
  return (is_pure, is_elemental)
end
export procedure_attributes

# Helper function to parse visibility
function visibility(line::AbstractString)::Visibility
  return occursin(r"PRIVATE", line) ? Types.Private : Types.Public
end
export visibility

# Helper function to parse type specifications
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
export type_spec

function get_date()::String
  return format(now(), "d U Y")
end
export get_date

end

