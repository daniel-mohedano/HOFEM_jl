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
end

_REGEXES = Dict(
  MODULE_START => r"MODULE (\w+)",
  MODULE_END  => r"END MODULE (\w+)",
  TYPE_START  => r"TYPE\s*::\s*(\w+)|^TYPE,\s?extends\(\w+\)\s*::\s*(\w+)",
  TYPE_END  => r"END TYPE (\w+)",
  MEMBER  => r"(\w+\(.+\)\w+)\s*::\s*(\w+(?:\([\w:]*\))?)(?:\s*=\s*[^!]*)?",
  GLOBAL_VAR  => r"TYPE\((\w+)\),\s*TARGET\s*::\s*(\w+)",
  SUBROUTINE_START  => r"SUBROUTINE (\w+)",
  SUBROUTINE_END  => r"END SUBROUTINE (\w+)",
  FUNCTION_START  => r"(\w+) FUNCTION ((\w+))\(\)",
  FUNCTION_END  => r"END FUNCTION (\w+)",
)

_START = r"^\s*"
_END = r"\s*$"

function reg_match(match_type::Matches, line::AbstractString)
  return match(_START * _REGEXES[match_type] * _END, line)
end
