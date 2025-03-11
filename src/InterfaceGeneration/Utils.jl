include("InterfaceTypes.jl")
using Dates: now, format

"""
  docheader(filename, modname, lang)

Generate the header for a document of the given language.
"""
function docheader(filename::AbstractString, modname::AbstractString, lang::Fortran)::String
  return ""
end

function docheader(filename::AbstractString, modname::AbstractString, lang::Julia)::AbstractString
  return """
  \"\"\"
  !----------------------------------------------------------------------------------------------------------------------
  !> file: $filename
  !> author: HOFEM_jl
  !> date: $(_getdate())
  !> description: Automatically generated Julia interface for $modname
  !----------------------------------------------------------------------------------------------------------------------
  \"\"\"
  """
end

function _getdate()::String
  return format(now(), "d U Y")
end
