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
  \tUSE iso_c_binding
  \tUSE $mod_name
  \t
  \tIMPLICIT NONE
  \t
  \tCONTAINS
  $custom_section

  $contents
  END MODULE $interface_name
  """
end

function t_module_structure_julia(contents::AbstractString, custom_section::AbstractString)::String
  return """
  _HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
  $custom_section

  $contents
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

function t_new_name(type_name::AbstractString)::String
  return "new_$(type_name)"
end

function t_free_name(type_name::AbstractString)::String
  return "free_$(type_name)"
end

function t_setter_name(type_name::AbstractString, member_name::AbstractString, lang::Julia)::String
  return t_setter_name(type_name, member_name, FORTRAN) * "!"
end

function t_getter_name(type_name::AbstractString, member_name::AbstractString, lang::Julia)::String
  return t_getter_name(type_name, member_name, FORTRAN)
end

function t_setter(type_name::AbstractString, member_name::AbstractString, member_type::IntrinsicType, member_inter_type::AbstractString, lang::Fortran)::String
  if isstring(member_type)
    return """
    SUBROUTINE $(t_setter_name(type_name, member_name, lang))(data_c_ptr, val) BIND(C)
    \tTYPE(C_PTR), VALUE :: data_c_ptr
    \tCHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
    \tTYPE($type_name), POINTER :: data
    \tINTEGER :: i = 1
    \t
    \tCALL c_f_pointer(data_c_ptr, data)
    \tDO
    \t\tIF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
    \t\tdata%$member_name(i:i) = val(i)
    \t\ti = i + 1
    \tEND DO
    \tDO WHILE (i <= LENGTH)
    \t\tdata%$member_name(i:i) = " "
    \t\ti = i + 1
    \tEND DO
    END SUBROUTINE $(t_setter_name(type_name, member_name, lang))
    """
  else
    return """
    SUBROUTINE $(t_setter_name(type_name, member_name, lang))(data_c_ptr, val) BIND(C)
    \tTYPE(C_PTR), VALUE :: data_c_ptr
    \t$member_inter_type, VALUE :: val
    \tTYPE($type_name), POINTER :: data
    \t
    \tCALL c_f_pointer(data_c_ptr, data)
    \tdata%$member_name = val
    END SUBROUTINE $(t_setter_name(type_name, member_name, lang))
    """
  end
end

function t_setter(type_name::AbstractString, member_name::AbstractString, member_type::IntrinsicType, member_inter_type::AbstractString, lang::Julia)::String
  if isstring(member_type)
    return """
    function set_$(member_name)!(obj::$(type_name), val::String)
    \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_setter_name(type_name, member_name, FORTRAN)))(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
    end
    """
  else
    return """
    function set_$(member_name)!(obj::$(type_name), val::$member_inter_type)
    \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_setter_name(type_name, member_name, FORTRAN)))(obj.handle::Ptr{Cvoid}, val::$member_inter_type)::Cvoid
    end
    """
  end
end

function t_getter(type_name::AbstractString, member_name::AbstractString, member_type::IntrinsicType, member_inter_type::AbstractString, lang::Fortran)::String
  if isstring(member_type)
    return """
    SUBROUTINE $(t_getter_name(type_name, member_name, lang))(data_c_ptr, string) BIND(C)
    \tTYPE(C_PTR), VALUE :: data_c_ptr
    \tCHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
    \tTYPE($type_name), POINTER :: data
    \tINTEGER :: i = 1
    \t
    \tCALL c_f_pointer(data_c_ptr, data)
    \tDO
    \t\tIF ((data%$member_name(i:i) == " ") .OR. (i == LENGTH)) EXIT
    \t\tstring(i) = data%$member_name(i:i)
    \t\ti = i + 1
    \tEND DO
    \tstring(i) = C_NULL_CHAR
    END SUBROUTINE $(t_getter_name(type_name, member_name, lang))
    """
  else
    return """
    FUNCTION $(t_getter_name(type_name, member_name, lang))(data_c_ptr) BIND(C)
    \tTYPE(C_PTR), VALUE :: data_c_ptr
    \t$member_inter_type :: $(t_getter_name(type_name, member_name, lang))
    \tTYPE($type_name), POINTER :: data
    \t
    \tCALL c_f_pointer(data_c_ptr, data)
    \t$(t_getter_name(type_name, member_name, lang)) = data%$member_name
    END FUNCTION $(t_getter_name(type_name, member_name, lang))
    """
  end
end

function t_getter(type_name::AbstractString, member_name::AbstractString, member_type::IntrinsicType, member_inter_type::AbstractString, lang::Julia)::String
  if isstring(member_type)
    return """
    function get_$(member_name)(obj::$(type_name), val::String)
    \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_name(type_name, member_name, FORTRAN)))(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
    end
    """
  else
    return """
    function get_$(member_name)(obj::$(type_name))::$member_inter_type
    \treturn @ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_name(type_name, member_name, FORTRAN)))(obj.handle::Ptr{Cvoid})::$member_inter_type
    end
    """
  end
end

function t_factory(type_name::AbstractString, lang::Fortran)::String
  return """
  FUNCTION $(t_new_name(type_name))() BIND(C)
  \tTYPE(C_PTR) :: $(t_new_name(type_name))
  \tTYPE($type_name), POINTER :: obj

  \tALLOCATE(obj)
  \t$(t_new_name(type_name)) = c_loc(obj)
  END FUNCTION $(t_new_name(type_name))

  SUBROUTINE $(t_free_name(type_name))(p) BIND(C)
  \tTYPE(C_PTR), VALUE :: p
  \tTYPE($type_name), POINTER :: obj

  \tCALL c_f_pointer(p, obj)
  \tIF (ASSOCIATED(obj)) DEALLOCATE(obj)
  END SUBROUTINE $(t_free_name(type_name))
  """
end

function t_factory(type_name::AbstractString, lang::Julia)::String
  return """
  struct $type_name
  \thandle::Ptr{Cvoid}
  end

  function $type_name()
  \treturn $type_name(@ccall _HOFEM_LIB_PATH.$(lowercase(t_new_name(type_name)))()::Ptr{Cvoid})
  end

  function Base.finalize(obj::$type_name)
  \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_free_name(type_name)))(obj.handle::Ptr{Cvoid})::Cvoid
  end
  """
end

function t_getter_module_var(var_name::AbstractString, type_name::AbstractString, lang::Fortran)::String
  return """
  FUNCTION $(t_getter_module_var_name(var_name))() BIND(C)
  \tTYPE(C_PTR) :: $(t_getter_module_var_name(var_name))
  \tTYPE($type_name), POINTER :: ptr
  \tptr => $var_name
  \t$(t_getter_module_var_name(var_name)) = c_loc(ptr)
  END FUNCTION $(t_getter_module_var_name(var_name))
  """
end

function t_getter_module_var(var_name::AbstractString, type_name::AbstractString, lang::Julia)::String
  return """
  function get_$(var_name)()::$(type_name)
  \treturn $(type_name)(@ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_module_var_name(var_name)))()::Ptr{Cvoid})
  end
  """
end

function t_type_print(type_name::AbstractString, member_names::Vector{<:AbstractString}, lang::Fortran)::String
  print_statements = []
  for member in member_names
    push!(print_statements, "\tPRINT *, \"$member:\", data%$member")
  end
  return """
  SUBROUTINE $(t_type_print_name(type_name))(data_c_ptr) BIND(C)
  \tTYPE(C_PTR), VALUE :: data_c_ptr
  \tTYPE($type_name), POINTER :: data
  \tCALL c_f_pointer(data_c_ptr, data)
  \tPRINT *, "$type_name"
  $(join(print_statements, "\n"))
  END SUBROUTINE $(t_type_print_name(type_name))
  """
end

function t_type_print(type_name::AbstractString, member_names::Vector{<:AbstractString}, lang::Julia)::String
  return """
  function print_$(type_name)(obj::$(type_name))
  \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_type_print_name(type_name)))(obj.handle::Ptr{Cvoid})::Cvoid
  end
  """
end

function t_procedure_wrapper_name(proc_name::AbstractString)::String
  return "jl_$(proc_name)"
end

function t_procedure_julia_call(proc_name::AbstractString, mod_name::AbstractString, is_direct_call::Bool, is_bind_c::Bool, args::Vector, ret_type::Union{String,Nothing})::String
  if is_direct_call
    if is_bind_c
      call_name = lowercase(proc_name)
    else
      call_name = lowercase(mod_name) * "_mp_" * lowercase(proc_name) * "_"
    end
  else
    call_name = t_procedure_wrapper_name(proc_name)
  end

  if isempty(args)
    arg_list = ""
    call_args = ""
  else
    arg_list = join(["$(arg.name)::$(arg.arg_type)" for arg in args], ", ")
    call_args = join([arg.call_str for arg in args], ", ")
  end
  ret_annotation = isnothing(ret_type) ? "::Cvoid" : "::$(ret_type)"

  return """
  function $(proc_name)($(arg_list))$(ret_annotation)
  \treturn @ccall _HOFEM_LIB_PATH.$(call_name)($(call_args))$(ret_annotation)
  end
  """
end

function t_procedure_fortran_wrapper(proc_name::AbstractString, args::Vector, ret_type::Union{String,Nothing}, conversions::Vector{String})::String
  wrapper_name = t_procedure_wrapper_name(proc_name)

  arg_names = isempty(args) ? "" : join([arg.name for arg in args], ", ")
  arg_decls = isempty(args) ? "" : join(["\t$(arg.fortran_decl)" for arg in args], "\n")
  conversion_code = isempty(conversions) ? "" : join(["\t$(conv)" for conv in conversions], "\n")
  call_args = isempty(args) ? "" : join([arg.call_name for arg in args], ", ")

  if isnothing(ret_type)
    return """
    SUBROUTINE $(wrapper_name)($(arg_names)) BIND(C)
    $(arg_decls)
    $(conversion_code)
    \t
    \tCALL $(proc_name)($(call_args))
    END SUBROUTINE $(wrapper_name)
    """
  else
    return """
    FUNCTION $(wrapper_name)($(arg_names)) BIND(C)
    $(arg_decls)
    $(conversion_code)
    \t$(ret_type) :: $(wrapper_name)
    \t
    \t$(wrapper_name) = $(proc_name)($(call_args))
    END FUNCTION $(wrapper_name)
    """
  end
end
