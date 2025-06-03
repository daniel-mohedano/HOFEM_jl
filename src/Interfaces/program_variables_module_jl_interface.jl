"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 3 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for program_variables_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""
_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")

function programCommonData_set_program_mode!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_program_mode(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_get_program_mode(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_program_mode(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_set_postprocess_mode!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_postprocess_mode(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_get_postprocess_mode(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_postprocess_mode(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_set_geometry_units!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_geometry_units(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_get_geometry_units(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_geometry_units(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_set_projectName!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_projectname(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_get_projectName(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_projectname(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_set_mesh_format!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_mesh_format(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_get_mesh_format(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_mesh_format(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_set_mesh_filename!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_mesh_filename(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_get_mesh_filename(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_mesh_filename(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_set_numRHS!(data_c_ptr::Ptr{Cvoid}, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_numrhs(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function programCommonData_get_numRHS(data_c_ptr::Ptr{Cvoid})::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_numrhs(data_c_ptr::Ptr{Cvoid})::Cint
end

function programCommonData_set_working_frequency_index!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_working_frequency_index(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function programCommonData_get_working_frequency_index(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_working_frequency_index(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function programCommonData_set_delta_time!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_delta_time(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function programCommonData_get_delta_time(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_delta_time(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function programCommonData_set_simulation_stop_time!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_simulation_stop_time(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function programCommonData_get_simulation_stop_time(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_simulation_stop_time(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function programCommonData_set_outofcore_megas!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_outofcore_megas(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function programCommonData_get_outofcore_megas(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_outofcore_megas(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function programCommonData_set_solver_type!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_solver_type(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_get_solver_type(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_solver_type(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function programCommonData_set_num_threads!(data_c_ptr::Ptr{Cvoid}, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_num_threads(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function programCommonData_get_num_threads(data_c_ptr::Ptr{Cvoid})::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_num_threads(data_c_ptr::Ptr{Cvoid})::Cint
end

function programCommonData_set_solver_percentage!(data_c_ptr::Ptr{Cvoid}, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_solver_percentage(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function programCommonData_get_solver_percentage(data_c_ptr::Ptr{Cvoid})::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_solver_percentage(data_c_ptr::Ptr{Cvoid})::Cint
end

function programCommonData_set_flevel_solver_percentage!(data_c_ptr::Ptr{Cvoid}, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_flevel_solver_percentage(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function programCommonData_get_flevel_solver_percentage(data_c_ptr::Ptr{Cvoid})::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_flevel_solver_percentage(data_c_ptr::Ptr{Cvoid})::Cint
end

function programCommonData_set_slevel_solver_percentage!(data_c_ptr::Ptr{Cvoid}, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_slevel_solver_percentage(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function programCommonData_get_slevel_solver_percentage(data_c_ptr::Ptr{Cvoid})::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_slevel_solver_percentage(data_c_ptr::Ptr{Cvoid})::Cint
end

function programCommonData_set_flevel_memory_MB!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_flevel_memory_mb(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function programCommonData_get_flevel_memory_MB(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_flevel_memory_mb(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function programCommonData_set_slevel_memory_MB!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_slevel_memory_mb(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function programCommonData_get_slevel_memory_MB(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_slevel_memory_mb(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function print_programCommonData(data_c_ptr::Ptr{Cvoid})
	@ccall _HOFEM_LIB_PATH.print_programcommondata(data_c_ptr::Ptr{Cvoid})::Cvoid
end

struct programCommonData
	handle::Ptr{Cvoid}
end

function programCommonData()
	return programCommonData(@ccall _HOFEM_LIB_PATH.new_programcommondata()::Ptr{Cvoid})
end

function Base.finalize(obj::programCommonData)
	@ccall _HOFEM_LIB_PATH.free_programcommondata(obj.handle::Ptr{Cvoid})::Cvoid
end

function gid_surface_sets_set_set_ID!(data_c_ptr::Ptr{Cvoid}, val::Cint)
	@ccall _HOFEM_LIB_PATH.gid_surface_sets_set_set_id(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function gid_surface_sets_get_set_ID(data_c_ptr::Ptr{Cvoid})::Cint
	return @ccall _HOFEM_LIB_PATH.gid_surface_sets_get_set_id(data_c_ptr::Ptr{Cvoid})::Cint
end

function print_gid_surface_sets(data_c_ptr::Ptr{Cvoid})
	@ccall _HOFEM_LIB_PATH.print_gid_surface_sets(data_c_ptr::Ptr{Cvoid})::Cvoid
end

struct gid_surface_sets
	handle::Ptr{Cvoid}
end

function gid_surface_sets()
	return gid_surface_sets(@ccall _HOFEM_LIB_PATH.new_gid_surface_sets()::Ptr{Cvoid})
end

function Base.finalize(obj::gid_surface_sets)
	@ccall _HOFEM_LIB_PATH.free_gid_surface_sets(obj.handle::Ptr{Cvoid})::Cvoid
end

function get_commonProperties()::Ptr{Cvoid}
	return @ccall _HOFEM_LIB_PATH.get_commonproperties()::Ptr{Cvoid}
end



#@C Anything inside this section will be preserved by the builder

#/@C
