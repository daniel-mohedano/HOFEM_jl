T_FORTRAN_COMMENT_PREFIX = "!"
T_JULIA_COMMENT_PREFIX = "#"
T_CUSTOM_SECTION_MARKER_BEGIN = "@C"
T_CUSTOM_SECTION_MARKER_END = "/@C"
T_CUSTOM_SECTION_INFO = "Anything inside this section will be preserved by the builder"

abstract type Lang end
struct Fortran <: Lang end
struct Julia <: Lang end

FORTRAN = Fortran()
JULIA = Julia()

function t_doc_header_fortran(date::AbstractString, mod_name::AbstractString)::String
  return """
  !----------------------------------------------------------------------------------------------------------------------
  !
  ! \$Id\$
  !
  !----------------------------------------------------------------------------------------------------------------------
  !
  !> @author
  !> Daniel Mohedano Rodríguez
  !
  !> @date
  !> $date
  !
  ! DESCRIPTION:
  !> Automatically generated Julia interface module for $mod_name
  !----------------------------------------------------------------------------------------------------------------------
  """
end

function t_doc_header_julia(date::AbstractString, mod_name::AbstractString)::String
  return """
  \"\"\"
  !----------------------------------------------------------------------------------------------------------------------
  !> @author
  !> Daniel Mohedano Rodríguez
  !
  !> @date
  !> $date
  !
  ! DESCRIPTION:
  !> Automatically generated Julia interface module for $mod_name
  !----------------------------------------------------------------------------------------------------------------------
  \"\"\"
  """
end

function t_module_structure_fortran(interface_name::AbstractString, mod_name::AbstractString, contents::AbstractString, custom_section::AbstractString)::String
  return """
  MODULE $interface_name
      USE iso_c_binding
      USE $mod_name

      IMPLICIT NONE

      CONTAINS
  $contents

  $custom_section

  END MODULE $interface_name
  """
end

function t_module_structure_julia(module_code::AbstractString, custom_section::AbstractString)::String
  return """
  _HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")

  $module_code

  $custom_section
  """
end

function t_setter_name(type_name::AbstractString, member_name::AbstractString, lang::Fortran)::String
  return "$(type_name)_set_$(member_name)"
end

function t_getter_name(type_name::AbstractString, member_name::AbstractString, lang::Fortran)::String
  return "$(type_name)_get_$(member_name)"
end

function t_getter_module_var_name(var_name::AbstractString)::String
  return "get_$(var_name)"
end

function t_type_print_name(type_name::AbstractString)::String
  return "print_$(type_name)"
end

function t_setter_name(type_name::AbstractString, member_name::AbstractString, lang::Julia)::String
  return t_setter_name(type_name, member_name, FORTRAN) * "!"
end

function t_getter_name(type_name::AbstractString, member_name::AbstractString, lang::Julia)::String
  return t_getter_name(type_name, member_name, FORTRAN)
end

function t_setter(type_name::AbstractString, member_name::AbstractString, member_type::AbstractString, member_inter_type::AbstractString, lang::Fortran)::String
  if lowercase(member_type) == "char" || lowercase(member_type) == "character"
    return """
    SUBROUTINE $(t_setter_name(type_name, member_name, lang))(data_c_ptr, val) BIND(C)
        TYPE(C_PTR), VALUE :: data_c_ptr
        CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
        TYPE($type_name), POINTER :: data
        INTEGER :: i = 1

        CALL c_f_pointer(data_c_ptr, data)
        DO
            IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
            data%$member_name(i:i) = val(i)
            i = i + 1
        END DO
        DO WHILE (i <= LENGTH)
            data%$member_name(i:i) = " "
            i = i + 1
        END DO
    END SUBROUTINE $(t_setter_name(type_name, member_name, lang))
    """
  else
    return """
    SUBROUTINE $(t_setter_name(type_name, member_name, lang))(data_c_ptr, val) BIND(C)
        TYPE(C_PTR), VALUE :: data_c_ptr
        $member_type($member_inter_type), VALUE :: val
        TYPE($type_name), POINTER :: data
        
        CALL c_f_pointer(data_c_ptr, data)
        data%$member_name = val
    END SUBROUTINE $(t_setter_name(type_name, member_name, lang))
    """
  end
end

function t_setter(type_name::AbstractString, member_name::AbstractString, member_type::AbstractString, member_inter_type::AbstractString, lang::Julia)::String
  if lowercase(member_type) == "char" || lowercase(member_type) == "character"
    return """
    function $(t_setter_name(type_name, member_name, lang))(data_c_ptr::Ptr{Cvoid}, val::String)
        @ccall _HOFEM_LIB_PATH.$(lowercase(t_setter_name(type_name, member_name, FORTRAN)))(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
    end
    """
  else
    return """
    function $(t_setter_name(type_name, member_name, lang))(data_c_ptr::Ptr{Cvoid}, val::$member_inter_type)
        @ccall _HOFEM_LIB_PATH.$(lowercase(t_setter_name(type_name, member_name, FORTRAN)))(data_c_ptr::Ptr{Cvoid}, val::$member_inter_type)::Cvoid
    end
    """
  end
end

function t_getter(type_name::AbstractString, member_name::AbstractString, member_type::AbstractString, member_inter_type::AbstractString, lang::Fortran)::String
  if lowercase(member_type) == "char" || lowercase(member_type) == "character"
    return """
    SUBROUTINE $(t_getter_name(type_name, member_name, lang))(data_c_ptr, string) BIND(C)
        TYPE(C_PTR), VALUE :: data_c_ptr
        CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
        TYPE($type_name), POINTER :: data
        INTEGER :: i = 1

        CALL c_f_pointer(data_c_ptr, data)
        DO
            IF ((data%$member_name(i:i) == " ") .OR. (i == LENGTH)) EXIT
            string(i) = data%$member_name(i:i)
            i = i + 1
        END DO
        string(i) = C_NULL_CHAR
    END SUBROUTINE $(t_getter_name(type_name, member_name, lang))
    """
  else
    return """
    FUNCTION $(t_getter_name(type_name, member_name, lang))(data_c_ptr) BIND(C)
        TYPE(C_PTR), VALUE :: data_c_ptr
        $member_type($member_inter_type) :: $(t_getter_name(type_name, member_name, lang))
        TYPE($type_name), POINTER :: data
        
        CALL c_f_pointer(data_c_ptr, data)
        $(t_getter_name(type_name, member_name, lang)) = data%$member_name
    END FUNCTION $(t_getter_name(type_name, member_name, lang))
    """
  end
end

function t_getter(type_name::AbstractString, member_name::AbstractString, member_type::AbstractString, member_inter_type::AbstractString, lang::Julia)::String
  if lowercase(member_type) == "char" || lowercase(member_type) == "character"
    return """
    function $(t_getter_name(type_name, member_name, lang))(data_c_ptr::Ptr{Cvoid}, val::String)
        @ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_name(type_name, member_name, FORTRAN)))(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
    end
    """
  else
    return """
    function $(t_getter_name(type_name, member_name, lang))(data_c_ptr::Ptr{Cvoid})::$member_inter_type
        return @ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_name(type_name, member_name, FORTRAN)))(data_c_ptr::Ptr{Cvoid})::$member_inter_type
    end
    """
  end
end

function t_getter_module_var(var_name::AbstractString, type_name::AbstractString, lang::Fortran)::String
  return """
  FUNCTION $(t_getter_module_var_name(var_name))() BIND(C)
      TYPE(C_PTR) :: $(t_getter_module_var_name(var_name))
      TYPE($type_name), POINTER :: ptr
      ptr => $var_name
      $(t_getter_module_var_name(var_name)) = c_loc(ptr)
  END FUNCTION $(t_getter_module_var_name(var_name))
  """
end

function t_getter_module_var(var_name::AbstractString, type_name::AbstractString, lang::Julia)::String
  return """
  function $(t_getter_module_var_name(var_name))()::Ptr{Cvoid}
      return @ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_module_var_name(var_name)))()::Ptr{Cvoid}
  end
  """
end

function t_type_print(type_name::AbstractString, member_names::Vector{<:AbstractString}, lang::Fortran)::String
  print_statements = ""
  for member in member_names
    print_statements *= "    PRINT *, \"$member:\", data%$member\n"
  end
  return """
  SUBROUTINE $(t_type_print_name(type_name))(data_c_ptr) BIND(C)
      TYPE(C_PTR), VALUE :: data_c_ptr
      TYPE($type_name), POINTER :: data
      CALL c_f_pointer(data_c_ptr, data)
      PRINT *, "$type_name"
  $print_statements
  END SUBROUTINE $(t_type_print_name(type_name))
  """
end

function t_type_print(type_name::AbstractString, member_names::Vector{<:AbstractString}, lang::Julia)::String
  return """
  function $(t_type_print_name(type_name))(data_c_ptr::Ptr{Cvoid})
      @ccall _HOFEM_LIB_PATH.$(lowercase(t_type_print_name(type_name)))(data_c_ptr::Ptr{Cvoid})::Cvoid
  end
  """
end
