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

_REGEXES = Dict(
  MODULE_START => r"MODULE (\w+)",
  MODULE_END  => r"END MODULE (\w+)",
  TYPE_START  => r"TYPE(?:\s*,\s*(?:(?:ABSTRACT|PUBLIC|PRIVATE|EXTENDS\((\w+)\))\s*,?\s*)*)?(?:\s*::\s*)?(\w+)",
  TYPE_END  => r"END TYPE (\w+)",
  MEMBER  => r"(?:(?:PUBLIC|PRIVATE)\s*::\s*)?(\w+(?:\([^)]*\))?)\s*::\s*(\w+)(?:\([\w:,]*\))?(?:\s*=\s*[^!]*)?",
  GLOBAL_VAR  => r"TYPE\((\w+)\)(?:\s*,\s*(?:TARGET|ALLOCATABLE|POINTER|PUBLIC|PRIVATE))?\s*::\s*(\w+)",
  SUBROUTINE_START  => r"(?:(?:PURE|ELEMENTAL)\s+)?SUBROUTINE\s+(\w+)\s*\(([\w\s,]*)\)",
  SUBROUTINE_END  => r"END SUBROUTINE (\w+)",
  FUNCTION_START  => r"(?:(?:PURE|ELEMENTAL)\s+)?(\w+(?:\([^)]*\))?)\s+FUNCTION\s+(\w+)\s*\(([\w\s,]*)\)",
  FUNCTION_END  => r"END FUNCTION (\w+)",
  VARIABLE_ATTRIBUTES => r"(?:PARAMETER|ALLOCATABLE|POINTER|TARGET|INTENT\((?:IN|OUT|INOUT)\)|DIMENSION\([^)]*\))",
  TYPE_ATTRIBUTES => r"(?:ABSTRACT|PUBLIC|PRIVATE|EXTENDS\((\w+)\))",
  VISIBILITY => r"(?:PUBLIC|PRIVATE)"
)

_START = r"^\s*"
_END = r"\s*$"

function reg_match(match_type::Matches, line::AbstractString)
  return match(_START * _REGEXES[match_type] * _END, line)
end

# Helper function to extract variable attributes from a line
function extract_var_attributes(line::AbstractString)::FVarAttributes
    attrs = FVarAttributes()
    
    # Check for parameter
    attrs.is_parameter = occursin(r"PARAMETER", line)
    
    # Check for allocatable
    attrs.is_allocatable = occursin(r"ALLOCATABLE", line)
    
    # Check for pointer
    attrs.is_pointer = occursin(r"POINTER", line)
    
    # Check for dimensions
    dim_match = match(r"DIMENSION\(([^)]*)\)", line)
    if dim_match !== nothing
        # Parse dimensions into a vector of Int or Symbol
        dims = String[]
        for dim in split(dim_match.captures[1], ",")
            push!(dims, strip(dim))
        end
        attrs.dimensions = dims
    end
    
    # Check for intent
    intent_match = match(r"INTENT\((IN|OUT|INOUT)\)", line)
    if intent_match !== nothing
        attrs.intent = Symbol(lowercase(intent_match.captures[1]))
    end
    
    return attrs
end

# Helper function to extract type attributes from a line
function extract_type_attributes(line::AbstractString)::FDerivedAttributes
    attrs = FDerivedAttributes()
    
    # Check for abstract
    attrs.is_abstract = occursin(r"ABSTRACT", line)
    
    # Check for extends
    extends_match = match(r"EXTENDS\((\w+)\)", line)
    if extends_match !== nothing
        attrs.extends = extends_match.captures[1]
    end
    
    # Check for visibility (default is public)
    attrs.is_public = !occursin(r"PRIVATE", line)
    
    return attrs
end

# Helper function to check if a procedure is pure or elemental
function extract_procedure_attributes(line::AbstractString)
    is_pure = occursin(r"PURE\s+", line)
    is_elemental = occursin(r"ELEMENTAL\s+", line)
    return (is_pure, is_elemental)
end

# Helper function to parse visibility
function extract_visibility(line::AbstractString)::Visibility
    return occursin(r"PRIVATE", line) ? Private : Public
end
