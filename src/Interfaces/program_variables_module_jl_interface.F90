!----------------------------------------------------------------------------------------------------------------------
!
! $Id$
!
!----------------------------------------------------------------------------------------------------------------------
!
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 29 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for program_variables_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
MODULE program_variables_module_jl_interface
	USE iso_c_binding
	USE program_variables_module
	
	IMPLICIT NONE
	
	CONTAINS
!@C Anything inside this section will be preserved by the builder

!/@C

		FUNCTION new_programCommonData() BIND(C)
			TYPE(C_PTR) :: new_programCommonData
			TYPE(programCommonData), POINTER :: obj
		
			ALLOCATE(obj)
			new_programCommonData = c_loc(obj)
		END FUNCTION new_programCommonData
		
		SUBROUTINE free_programCommonData(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(programCommonData), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_programCommonData
		
		SUBROUTINE programCommonData_set_program_mode(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%program_mode(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%program_mode(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE programCommonData_set_program_mode
		
		SUBROUTINE programCommonData_get_program_mode(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%program_mode(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%program_mode(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE programCommonData_get_program_mode
		
		SUBROUTINE programCommonData_set_postprocess_mode(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%postprocess_mode(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%postprocess_mode(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE programCommonData_set_postprocess_mode
		
		SUBROUTINE programCommonData_get_postprocess_mode(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%postprocess_mode(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%postprocess_mode(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE programCommonData_get_postprocess_mode
		
		SUBROUTINE programCommonData_set_geometry_units(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%geometry_units(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%geometry_units(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE programCommonData_set_geometry_units
		
		SUBROUTINE programCommonData_get_geometry_units(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%geometry_units(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%geometry_units(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE programCommonData_get_geometry_units
		
		SUBROUTINE programCommonData_set_projectName(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%projectName(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%projectName(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE programCommonData_set_projectName
		
		SUBROUTINE programCommonData_get_projectName(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%projectName(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%projectName(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE programCommonData_get_projectName
		
		SUBROUTINE programCommonData_set_mesh_format(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%mesh_format(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%mesh_format(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE programCommonData_set_mesh_format
		
		SUBROUTINE programCommonData_get_mesh_format(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%mesh_format(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%mesh_format(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE programCommonData_get_mesh_format
		
		SUBROUTINE programCommonData_set_mesh_filename(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%mesh_filename(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%mesh_filename(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE programCommonData_set_mesh_filename
		
		SUBROUTINE programCommonData_get_mesh_filename(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%mesh_filename(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%mesh_filename(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE programCommonData_get_mesh_filename
		
		SUBROUTINE programCommonData_set_numRHS(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%numRHS = val
		END SUBROUTINE programCommonData_set_numRHS
		
		FUNCTION programCommonData_get_numRHS(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: programCommonData_get_numRHS
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_numRHS = data%numRHS
		END FUNCTION programCommonData_get_numRHS
		
		SUBROUTINE programCommonData_set_working_frequency_index(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%working_frequency_index = val
		END SUBROUTINE programCommonData_set_working_frequency_index
		
		FUNCTION programCommonData_get_working_frequency_index(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: programCommonData_get_working_frequency_index
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_working_frequency_index = data%working_frequency_index
		END FUNCTION programCommonData_get_working_frequency_index
		
		SUBROUTINE programCommonData_set_delta_time(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%delta_time = val
		END SUBROUTINE programCommonData_set_delta_time
		
		FUNCTION programCommonData_get_delta_time(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: programCommonData_get_delta_time
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_delta_time = data%delta_time
		END FUNCTION programCommonData_get_delta_time
		
		SUBROUTINE programCommonData_set_simulation_stop_time(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%simulation_stop_time = val
		END SUBROUTINE programCommonData_set_simulation_stop_time
		
		FUNCTION programCommonData_get_simulation_stop_time(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: programCommonData_get_simulation_stop_time
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_simulation_stop_time = data%simulation_stop_time
		END FUNCTION programCommonData_get_simulation_stop_time
		
		SUBROUTINE programCommonData_set_outofcore_megas(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%outofcore_megas = val
		END SUBROUTINE programCommonData_set_outofcore_megas
		
		FUNCTION programCommonData_get_outofcore_megas(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: programCommonData_get_outofcore_megas
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_outofcore_megas = data%outofcore_megas
		END FUNCTION programCommonData_get_outofcore_megas
		
		SUBROUTINE programCommonData_set_solver_type(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%solver_type(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%solver_type(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE programCommonData_set_solver_type
		
		SUBROUTINE programCommonData_get_solver_type(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(programCommonData), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%solver_type(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%solver_type(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE programCommonData_get_solver_type
		
		SUBROUTINE programCommonData_set_num_threads(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%num_threads = val
		END SUBROUTINE programCommonData_set_num_threads
		
		FUNCTION programCommonData_get_num_threads(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: programCommonData_get_num_threads
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_num_threads = data%num_threads
		END FUNCTION programCommonData_get_num_threads
		
		SUBROUTINE programCommonData_set_solver_percentage(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%solver_percentage = val
		END SUBROUTINE programCommonData_set_solver_percentage
		
		FUNCTION programCommonData_get_solver_percentage(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: programCommonData_get_solver_percentage
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_solver_percentage = data%solver_percentage
		END FUNCTION programCommonData_get_solver_percentage
		
		SUBROUTINE programCommonData_set_flevel_solver_percentage(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%flevel_solver_percentage = val
		END SUBROUTINE programCommonData_set_flevel_solver_percentage
		
		FUNCTION programCommonData_get_flevel_solver_percentage(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: programCommonData_get_flevel_solver_percentage
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_flevel_solver_percentage = data%flevel_solver_percentage
		END FUNCTION programCommonData_get_flevel_solver_percentage
		
		SUBROUTINE programCommonData_set_slevel_solver_percentage(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%slevel_solver_percentage = val
		END SUBROUTINE programCommonData_set_slevel_solver_percentage
		
		FUNCTION programCommonData_get_slevel_solver_percentage(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: programCommonData_get_slevel_solver_percentage
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_slevel_solver_percentage = data%slevel_solver_percentage
		END FUNCTION programCommonData_get_slevel_solver_percentage
		
		SUBROUTINE programCommonData_set_flevel_memory_MB(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%flevel_memory_MB = val
		END SUBROUTINE programCommonData_set_flevel_memory_MB
		
		FUNCTION programCommonData_get_flevel_memory_MB(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: programCommonData_get_flevel_memory_MB
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_flevel_memory_MB = data%flevel_memory_MB
		END FUNCTION programCommonData_get_flevel_memory_MB
		
		SUBROUTINE programCommonData_set_slevel_memory_MB(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%slevel_memory_MB = val
		END SUBROUTINE programCommonData_set_slevel_memory_MB
		
		FUNCTION programCommonData_get_slevel_memory_MB(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: programCommonData_get_slevel_memory_MB
			TYPE(programCommonData), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			programCommonData_get_slevel_memory_MB = data%slevel_memory_MB
		END FUNCTION programCommonData_get_slevel_memory_MB
		
		SUBROUTINE print_programCommonData(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(programCommonData), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "programCommonData"
			PRINT *, "program_mode:", data%program_mode
			PRINT *, "postprocess_mode:", data%postprocess_mode
			PRINT *, "geometry_units:", data%geometry_units
			PRINT *, "projectName:", data%projectName
			PRINT *, "mesh_format:", data%mesh_format
			PRINT *, "mesh_filename:", data%mesh_filename
			PRINT *, "numRHS:", data%numRHS
			PRINT *, "working_frequency_index:", data%working_frequency_index
			PRINT *, "frequency_array:", data%frequency_array
			PRINT *, "delta_time:", data%delta_time
			PRINT *, "simulation_stop_time:", data%simulation_stop_time
			PRINT *, "outofcore_megas:", data%outofcore_megas
			PRINT *, "solver_type:", data%solver_type
			PRINT *, "num_threads:", data%num_threads
			PRINT *, "solver_percentage:", data%solver_percentage
			PRINT *, "flevel_solver_percentage:", data%flevel_solver_percentage
			PRINT *, "slevel_solver_percentage:", data%slevel_solver_percentage
			PRINT *, "flevel_memory_MB:", data%flevel_memory_MB
			PRINT *, "slevel_memory_MB:", data%slevel_memory_MB
		END SUBROUTINE print_programCommonData
		
		FUNCTION new_gid_surface_sets() BIND(C)
			TYPE(C_PTR) :: new_gid_surface_sets
			TYPE(gid_surface_sets), POINTER :: obj
		
			ALLOCATE(obj)
			new_gid_surface_sets = c_loc(obj)
		END FUNCTION new_gid_surface_sets
		
		SUBROUTINE free_gid_surface_sets(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(gid_surface_sets), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_gid_surface_sets
		
		SUBROUTINE gid_surface_sets_set_set_ID(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(gid_surface_sets), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%set_ID = val
		END SUBROUTINE gid_surface_sets_set_set_ID
		
		FUNCTION gid_surface_sets_get_set_ID(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: gid_surface_sets_get_set_ID
			TYPE(gid_surface_sets), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			gid_surface_sets_get_set_ID = data%set_ID
		END FUNCTION gid_surface_sets_get_set_ID
		
		SUBROUTINE print_gid_surface_sets(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(gid_surface_sets), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "gid_surface_sets"
			PRINT *, "set_ID:", data%set_ID
			PRINT *, "set_info:", data%set_info
		END SUBROUTINE print_gid_surface_sets
		
		FUNCTION get_commonProperties() BIND(C)
			TYPE(C_PTR) :: get_commonProperties
			TYPE(programCommonData), POINTER :: ptr
			ptr => commonProperties
			get_commonProperties = c_loc(ptr)
		END FUNCTION get_commonProperties
		

END MODULE program_variables_module_jl_interface
