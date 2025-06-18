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
    function $(t_setter_name(type_name, member_name, lang))(data_c_ptr::Ptr{Cvoid}, val::String)
    \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_setter_name(type_name, member_name, FORTRAN)))(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
    end
    """
  else
    return """
    function $(t_setter_name(type_name, member_name, lang))(data_c_ptr::Ptr{Cvoid}, val::$member_inter_type)
    \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_setter_name(type_name, member_name, FORTRAN)))(data_c_ptr::Ptr{Cvoid}, val::$member_inter_type)::Cvoid
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
    function $(t_getter_name(type_name, member_name, lang))(data_c_ptr::Ptr{Cvoid}, val::String)
    \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_name(type_name, member_name, FORTRAN)))(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
    end
    """
  else
    return """
    function $(t_getter_name(type_name, member_name, lang))(data_c_ptr::Ptr{Cvoid})::$member_inter_type
    \treturn @ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_name(type_name, member_name, FORTRAN)))(data_c_ptr::Ptr{Cvoid})::$member_inter_type
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
  function $(t_getter_module_var_name(var_name))()::Ptr{Cvoid}
  \treturn @ccall _HOFEM_LIB_PATH.$(lowercase(t_getter_module_var_name(var_name)))()::Ptr{Cvoid}
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
  function $(t_type_print_name(type_name))(data_c_ptr::Ptr{Cvoid})
  \t@ccall _HOFEM_LIB_PATH.$(lowercase(t_type_print_name(type_name)))(data_c_ptr::Ptr{Cvoid})::Cvoid
  end
  """
end

function default_procedure_symbol(modname::AbstractString, procname::AbstractString; compiler="intel")::String
  if lowercase(compiler) == "intel"
    return lowercase(modname) * "_mp_" * lowercase(procname) * "__"
  elseif lowercase(compiler) == "gcc"
    return "__" * lowercase(modname) * "_MOD_" * lowercase(procname)
  else
    return lowercase(procname)
  end
end

wrapper_symbol(proc::AbstractString) = "call_" * lowercase(proc)

# returns
# - declaration lines
# - name to be passed to original routine, or nothing
# - the conversion statements (if needed)
function _wrapper_stub(v::Variable, is_return::Bool=false)::Tuple{String,AbstractString,AbstractString}
  # todo: check intent logic for return types
  f_type = fortran_type(v.type)
  if is_interoperable(v)
    intent = is_return ? "INTENT(OUT)" : "INTENT(IN)"
    if v.attributes.intent === "value"
      decl = "\t$f_type, VALUE :: $(v.name)"
    else
      if v.attributes.dimensions === nothing
        decl = "\t$f_type, $intent :: $(v.name)"
      else
        decl = "\t$f_type, DIMENSION(*) :: $(v.name)"
      end
    end

    return decl, v.name, ""
  else
    # todo: the original fortan type
    temp_name = "f_" * v.name
    decl = "\tTYPE(C_PTR), VALUE :: $(v.name)\n $(v.type.name)($f_type) :: $temp_name"

    return decl, temp_name, "\tCALL c_f_pointer($(v.name), $temp_name)"
  end
end

function t_call_routine(proc::Procedure, lang::Fortran)::String
  # todo: check if it is actually needed to generate or not
  name = wrapper_symbol(proc.name)
  arg_list = join([v.name for v in proc.args], ", ")
  stubs = [_wrapper_stub(v) for v in proc.args]
  decls = join([s[1] for s in stubs], "\n")
  call_args = join([s[2] for s in stubs], ", ")
  conversions = join([s[3] for s in stubs], "\n")

  if isnothing(proc.ret)
    return """
    SUBROUTINE $name($arg_list) BIND(C)
    $decls
    $conversions
    CALL $(proc.name)($call_args)
    END SUBROUTINE $name
    """
  else
    ret_stub = _wrapper_stub(proc.ret, true)
    decls *= ret_stub[1]

    return """
    FUNCTION $name($arg_list) BIND(C)
    $decls
    $conversions
    $(ret_stub[2]) = $(proc.name)($call_args)
    END FUNCTION $name
    """
  end
end

function t_call_routine(proc::Procedure, lang::Julia)::String
  name = wrapper_symbol(proc.name)
  args = [v.name for v in proc.args]
  types = [julia_type(v) for v in proc.args]
  inter_types = [startswith(t, "Ptr") ? t : "Ptr{$t}" for t in types]
  arg_list = join(["$n::$t" for (n, t) in zip(args, inter_types)], ", ")

  if isnothing(proc.ret)
    return """
    function $name($arg_list)
    \t@ccall _HOFEM_LIB_PATH.$name($arg_list)
    end
    """
  else
    ret_type = julia_type(proc.ret)
    return """
    function $name($arg_list)::$ret_type
    \treturn @ccall _HOFEM_LIB_PATH.$name($arg_list)::$ret_type
    end
    """
  end
end
