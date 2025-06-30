"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for fake_HOFEM_library_mod_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
#@C Anything inside this section will be preserved by the builder

#/@C

struct fakeObject_t
	handle::Ptr{Cvoid}
end

function fakeObject_t()
	return fakeObject_t(@ccall _HOFEM_LIB_PATH.new_fakeobject_t()::Ptr{Cvoid})
end

function Base.finalize(obj::fakeObject_t)
	@ccall _HOFEM_LIB_PATH.free_fakeobject_t(obj.handle::Ptr{Cvoid})::Cvoid
end

function set_myinteger!(obj::fakeObject_t, val::Cint)
	@ccall _HOFEM_LIB_PATH.fakeobject_t_set_myinteger(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_myinteger(obj::fakeObject_t)::Cint
	return @ccall _HOFEM_LIB_PATH.fakeobject_t_get_myinteger(obj.handle::Ptr{Cvoid})::Cint
end

function set_mymessage!(obj::fakeObject_t, val::String)
	@ccall _HOFEM_LIB_PATH.fakeobject_t_set_mymessage(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_mymessage(obj::fakeObject_t, val::String)
	@ccall _HOFEM_LIB_PATH.fakeobject_t_get_mymessage(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function print_fakeObject_t(obj::fakeObject_t)
	@ccall _HOFEM_LIB_PATH.print_fakeobject_t(obj.handle::Ptr{Cvoid})::Cvoid
end

function print_fakeObject(thisfakeObject::fakeObject_t)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_print_fakeObject(thisfakeObject.handle::Ptr{Cvoid})::Cvoid
end


