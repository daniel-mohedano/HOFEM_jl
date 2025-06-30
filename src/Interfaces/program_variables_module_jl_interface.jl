"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for program_variables_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
#@C Anything inside this section will be preserved by the builder

#/@C

struct programCommonData
	handle::Ptr{Cvoid}
end

function programCommonData()
	return programCommonData(@ccall _HOFEM_LIB_PATH.new_programcommondata()::Ptr{Cvoid})
end

function Base.finalize(obj::programCommonData)
	@ccall _HOFEM_LIB_PATH.free_programcommondata(obj.handle::Ptr{Cvoid})::Cvoid
end

function set_program_mode!(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_program_mode(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_program_mode(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_program_mode(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_postprocess_mode!(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_postprocess_mode(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_postprocess_mode(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_postprocess_mode(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_geometry_units!(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_geometry_units(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_geometry_units(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_geometry_units(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_projectName!(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_projectname(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_projectName(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_projectname(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_mesh_format!(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_mesh_format(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_mesh_format(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_mesh_format(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_mesh_filename!(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_mesh_filename(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_mesh_filename(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_mesh_filename(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_numRHS!(obj::programCommonData, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_numrhs(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_numRHS(obj::programCommonData)::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_numrhs(obj.handle::Ptr{Cvoid})::Cint
end

function set_working_frequency_index!(obj::programCommonData, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_working_frequency_index(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_working_frequency_index(obj::programCommonData)::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_working_frequency_index(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_delta_time!(obj::programCommonData, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_delta_time(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_delta_time(obj::programCommonData)::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_delta_time(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_simulation_stop_time!(obj::programCommonData, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_simulation_stop_time(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_simulation_stop_time(obj::programCommonData)::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_simulation_stop_time(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_outofcore_megas!(obj::programCommonData, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_outofcore_megas(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_outofcore_megas(obj::programCommonData)::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_outofcore_megas(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_solver_type!(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_solver_type(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_solver_type(obj::programCommonData, val::String)
	@ccall _HOFEM_LIB_PATH.programcommondata_get_solver_type(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_num_threads!(obj::programCommonData, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_num_threads(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_num_threads(obj::programCommonData)::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_num_threads(obj.handle::Ptr{Cvoid})::Cint
end

function set_solver_percentage!(obj::programCommonData, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_solver_percentage(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_solver_percentage(obj::programCommonData)::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_solver_percentage(obj.handle::Ptr{Cvoid})::Cint
end

function set_flevel_solver_percentage!(obj::programCommonData, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_flevel_solver_percentage(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_flevel_solver_percentage(obj::programCommonData)::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_flevel_solver_percentage(obj.handle::Ptr{Cvoid})::Cint
end

function set_slevel_solver_percentage!(obj::programCommonData, val::Cint)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_slevel_solver_percentage(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_slevel_solver_percentage(obj::programCommonData)::Cint
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_slevel_solver_percentage(obj.handle::Ptr{Cvoid})::Cint
end

function set_flevel_memory_MB!(obj::programCommonData, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_flevel_memory_mb(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_flevel_memory_MB(obj::programCommonData)::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_flevel_memory_mb(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_slevel_memory_MB!(obj::programCommonData, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.programcommondata_set_slevel_memory_mb(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_slevel_memory_MB(obj::programCommonData)::Cdouble
	return @ccall _HOFEM_LIB_PATH.programcommondata_get_slevel_memory_mb(obj.handle::Ptr{Cvoid})::Cdouble
end

function print_programCommonData(obj::programCommonData)
	@ccall _HOFEM_LIB_PATH.print_programcommondata(obj.handle::Ptr{Cvoid})::Cvoid
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

function set_set_ID!(obj::gid_surface_sets, val::Cint)
	@ccall _HOFEM_LIB_PATH.gid_surface_sets_set_set_id(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_set_ID(obj::gid_surface_sets)::Cint
	return @ccall _HOFEM_LIB_PATH.gid_surface_sets_get_set_id(obj.handle::Ptr{Cvoid})::Cint
end

function print_gid_surface_sets(obj::gid_surface_sets)
	@ccall _HOFEM_LIB_PATH.print_gid_surface_sets(obj.handle::Ptr{Cvoid})::Cvoid
end

function get_commonProperties()::programCommonData
	return programCommonData(@ccall _HOFEM_LIB_PATH.get_commonproperties()::Ptr{Cvoid})
end

function deallocate_HOFEM_data_memory()::Cvoid
	return @ccall _HOFEM_LIB_PATH.program_variables_module_mp_deallocate_hofem_data_memory_()::Cvoid
end

function initialize_K0_and_current_working_frequency(frequency_index::Ref{Cint})::Cvoid
	return @ccall _HOFEM_LIB_PATH.program_variables_module_mp_initialize_k0_and_current_working_frequency_(frequency_index::Ref{Cint})::Cvoid
end

function incr_program_data_number_of_RHS(rhs::Ref{Cint})::Cvoid
	return @ccall _HOFEM_LIB_PATH.program_variables_module_mp_incr_program_data_number_of_rhs_(rhs::Ref{Cint})::Cvoid
end

function get_current_working_frequency()::Cdouble
	return @ccall _HOFEM_LIB_PATH.program_variables_module_mp_get_current_working_frequency_()::Cdouble
end

function get_current_working_frequency_in_Hz()::Cdouble
	return @ccall _HOFEM_LIB_PATH.program_variables_module_mp_get_current_working_frequency_in_hz_()::Cdouble
end

function get_program_data_solver_type()::Ptr{Cchar}
	return @ccall _HOFEM_LIB_PATH.program_variables_module_mp_get_program_data_solver_type_()::Ptr{Cchar}
end

function get_program_data_maximum_MB_of_RAM()::Cdouble
	return @ccall _HOFEM_LIB_PATH.program_variables_module_mp_get_program_data_maximum_mb_of_ram_()::Cdouble
end

function get_program_data_num_threads()::Cint
	return @ccall _HOFEM_LIB_PATH.program_variables_module_mp_get_program_data_num_threads_()::Cint
end


