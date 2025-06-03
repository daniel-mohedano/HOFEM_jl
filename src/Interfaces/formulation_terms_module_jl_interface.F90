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
!> 3 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for formulation_terms_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
MODULE formulation_terms_module_jl_interface
	USE iso_c_binding
	USE formulation_terms_module
	
	IMPLICIT NONE
	
	CONTAINS
		SUBROUTINE thermal_formulation_set_delta_sim_time(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%delta_sim_time = val
		END SUBROUTINE thermal_formulation_set_delta_sim_time
		
		FUNCTION thermal_formulation_get_delta_sim_time(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: thermal_formulation_get_delta_sim_time
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			thermal_formulation_get_delta_sim_time = data%delta_sim_time
		END FUNCTION thermal_formulation_get_delta_sim_time
		
		SUBROUTINE thermal_formulation_set_stop_sim_time(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%stop_sim_time = val
		END SUBROUTINE thermal_formulation_set_stop_sim_time
		
		FUNCTION thermal_formulation_get_stop_sim_time(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: thermal_formulation_get_stop_sim_time
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			thermal_formulation_get_stop_sim_time = data%stop_sim_time
		END FUNCTION thermal_formulation_get_stop_sim_time
		
		SUBROUTINE thermal_formulation_set_current_sim_time(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%current_sim_time = val
		END SUBROUTINE thermal_formulation_set_current_sim_time
		
		FUNCTION thermal_formulation_get_current_sim_time(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: thermal_formulation_get_current_sim_time
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			thermal_formulation_get_current_sim_time = data%current_sim_time
		END FUNCTION thermal_formulation_get_current_sim_time
		
		SUBROUTINE thermal_formulation_set_total_time_steps(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%total_time_steps = val
		END SUBROUTINE thermal_formulation_set_total_time_steps
		
		FUNCTION thermal_formulation_get_total_time_steps(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: thermal_formulation_get_total_time_steps
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			thermal_formulation_get_total_time_steps = data%total_time_steps
		END FUNCTION thermal_formulation_get_total_time_steps
		
		SUBROUTINE thermal_formulation_set_step_tolerance(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE), VALUE :: val
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%step_tolerance = val
		END SUBROUTINE thermal_formulation_set_step_tolerance
		
		FUNCTION thermal_formulation_get_step_tolerance(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			REAL(C_DOUBLE) :: thermal_formulation_get_step_tolerance
			TYPE(thermal_formulation), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			thermal_formulation_get_step_tolerance = data%step_tolerance
		END FUNCTION thermal_formulation_get_step_tolerance
		
		SUBROUTINE thermal_formulation_set_time_step_strategy(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(thermal_formulation), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%time_step_strategy(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%time_step_strategy(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE thermal_formulation_set_time_step_strategy
		
		SUBROUTINE thermal_formulation_get_time_step_strategy(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(thermal_formulation), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%time_step_strategy(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%time_step_strategy(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE thermal_formulation_get_time_step_strategy
		
		SUBROUTINE print_thermal_formulation(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(thermal_formulation), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "thermal_formulation"
			PRINT *, "delta_sim_time:", data%delta_sim_time
			PRINT *, "stop_sim_time:", data%stop_sim_time
			PRINT *, "current_sim_time:", data%current_sim_time
			PRINT *, "total_time_steps:", data%total_time_steps
			PRINT *, "previous_max_error:", data%previous_max_error
			PRINT *, "step_tolerance:", data%step_tolerance
			PRINT *, "time_step_strategy:", data%time_step_strategy
		
		END SUBROUTINE print_thermal_formulation
		
		FUNCTION new_thermal_formulation() BIND(C)
			TYPE(C_PTR) :: new_thermal_formulation
			TYPE(thermal_formulation), POINTER :: obj
		
			ALLOCATE(obj)
			new_thermal_formulation = c_loc(obj)
		END FUNCTION new_thermal_formulation
		
		SUBROUTINE free_thermal_formulation(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(thermal_formulation), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_thermal_formulation
		
		SUBROUTINE print_double_curl_wave_formulation(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(double_curl_wave_formulation), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "double_curl_wave_formulation"
		
		END SUBROUTINE print_double_curl_wave_formulation
		
		FUNCTION new_double_curl_wave_formulation() BIND(C)
			TYPE(C_PTR) :: new_double_curl_wave_formulation
			TYPE(double_curl_wave_formulation), POINTER :: obj
		
			ALLOCATE(obj)
			new_double_curl_wave_formulation = c_loc(obj)
		END FUNCTION new_double_curl_wave_formulation
		
		SUBROUTINE free_double_curl_wave_formulation(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(double_curl_wave_formulation), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_double_curl_wave_formulation
		
		SUBROUTINE FormulationConstants_set_bilGradFiGradFi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilGradFiGradFi = val
		END SUBROUTINE FormulationConstants_set_bilGradFiGradFi
		
		FUNCTION FormulationConstants_get_bilGradFiGradFi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_bilGradFiGradFi
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_bilGradFiGradFi = data%bilGradFiGradFi
		END FUNCTION FormulationConstants_get_bilGradFiGradFi
		
		SUBROUTINE FormulationConstants_set_bilFiFi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilFiFi = val
		END SUBROUTINE FormulationConstants_set_bilFiFi
		
		FUNCTION FormulationConstants_get_bilFiFi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_bilFiFi
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_bilFiFi = data%bilFiFi
		END FUNCTION FormulationConstants_get_bilFiFi
		
		SUBROUTINE FormulationConstants_set_linFiFunction(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%linFiFunction = val
		END SUBROUTINE FormulationConstants_set_linFiFunction
		
		FUNCTION FormulationConstants_get_linFiFunction(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_linFiFunction
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_linFiFunction = data%linFiFunction
		END FUNCTION FormulationConstants_get_linFiFunction
		
		SUBROUTINE FormulationConstants_set_linBoundFiFunction(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%linBoundFiFunction = val
		END SUBROUTINE FormulationConstants_set_linBoundFiFunction
		
		FUNCTION FormulationConstants_get_linBoundFiFunction(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_linBoundFiFunction
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_linBoundFiFunction = data%linBoundFiFunction
		END FUNCTION FormulationConstants_get_linBoundFiFunction
		
		SUBROUTINE FormulationConstants_set_bilBoundFiFi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilBoundFiFi = val
		END SUBROUTINE FormulationConstants_set_bilBoundFiFi
		
		FUNCTION FormulationConstants_get_bilBoundFiFi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_bilBoundFiFi
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_bilBoundFiFi = data%bilBoundFiFi
		END FUNCTION FormulationConstants_get_bilBoundFiFi
		
		SUBROUTINE FormulationConstants_set_bilRotNiRotNi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilRotNiRotNi = val
		END SUBROUTINE FormulationConstants_set_bilRotNiRotNi
		
		FUNCTION FormulationConstants_get_bilRotNiRotNi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_bilRotNiRotNi
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_bilRotNiRotNi = data%bilRotNiRotNi
		END FUNCTION FormulationConstants_get_bilRotNiRotNi
		
		SUBROUTINE FormulationConstants_set_bilNiNi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilNiNi = val
		END SUBROUTINE FormulationConstants_set_bilNiNi
		
		FUNCTION FormulationConstants_get_bilNiNi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_bilNiNi
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_bilNiNi = data%bilNiNi
		END FUNCTION FormulationConstants_get_bilNiNi
		
		SUBROUTINE FormulationConstants_set_linBoundNiFunction(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%linBoundNiFunction = val
		END SUBROUTINE FormulationConstants_set_linBoundNiFunction
		
		FUNCTION FormulationConstants_get_linBoundNiFunction(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_linBoundNiFunction
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_linBoundNiFunction = data%linBoundNiFunction
		END FUNCTION FormulationConstants_get_linBoundNiFunction
		
		SUBROUTINE FormulationConstants_set_bilBoundNiNi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilBoundNiNi = val
		END SUBROUTINE FormulationConstants_set_bilBoundNiNi
		
		FUNCTION FormulationConstants_get_bilBoundNiNi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_bilBoundNiNi
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_bilBoundNiNi = data%bilBoundNiNi
		END FUNCTION FormulationConstants_get_bilBoundNiNi
		
		SUBROUTINE FormulationConstants_set_linNiFunction(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%linNiFunction = val
		END SUBROUTINE FormulationConstants_set_linNiFunction
		
		FUNCTION FormulationConstants_get_linNiFunction(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_linNiFunction
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_linNiFunction = data%linNiFunction
		END FUNCTION FormulationConstants_get_linNiFunction
		
		SUBROUTINE FormulationConstants_set_bilNiGradFi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX), VALUE :: val
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilNiGradFi = val
		END SUBROUTINE FormulationConstants_set_bilNiGradFi
		
		FUNCTION FormulationConstants_get_bilNiGradFi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			COMPLEX(C_DOUBLE_COMPLEX) :: FormulationConstants_get_bilNiGradFi
			TYPE(FormulationConstants), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationConstants_get_bilNiGradFi = data%bilNiGradFi
		END FUNCTION FormulationConstants_get_bilNiGradFi
		
		SUBROUTINE print_FormulationConstants(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(FormulationConstants), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "FormulationConstants"
			PRINT *, "bilGradFiGradFi:", data%bilGradFiGradFi
			PRINT *, "bilFiFi:", data%bilFiFi
			PRINT *, "linFiFunction:", data%linFiFunction
			PRINT *, "linBoundFiFunction:", data%linBoundFiFunction
			PRINT *, "bilBoundFiFi:", data%bilBoundFiFi
			PRINT *, "bilRotNiRotNi:", data%bilRotNiRotNi
			PRINT *, "bilNiNi:", data%bilNiNi
			PRINT *, "linBoundNiFunction:", data%linBoundNiFunction
			PRINT *, "bilBoundNiNi:", data%bilBoundNiNi
			PRINT *, "linNiFunction:", data%linNiFunction
			PRINT *, "bilNiGradFi:", data%bilNiGradFi
		
		END SUBROUTINE print_FormulationConstants
		
		FUNCTION new_FormulationConstants() BIND(C)
			TYPE(C_PTR) :: new_FormulationConstants
			TYPE(FormulationConstants), POINTER :: obj
		
			ALLOCATE(obj)
			new_FormulationConstants = c_loc(obj)
		END FUNCTION new_FormulationConstants
		
		SUBROUTINE free_FormulationConstants(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(FormulationConstants), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_FormulationConstants
		
		SUBROUTINE FormulationTerms_set_bilGradFiGradFi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilGradFiGradFi = val
		END SUBROUTINE FormulationTerms_set_bilGradFiGradFi
		
		FUNCTION FormulationTerms_get_bilGradFiGradFi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_bilGradFiGradFi
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_bilGradFiGradFi = data%bilGradFiGradFi
		END FUNCTION FormulationTerms_get_bilGradFiGradFi
		
		SUBROUTINE FormulationTerms_set_bilFiFi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilFiFi = val
		END SUBROUTINE FormulationTerms_set_bilFiFi
		
		FUNCTION FormulationTerms_get_bilFiFi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_bilFiFi
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_bilFiFi = data%bilFiFi
		END FUNCTION FormulationTerms_get_bilFiFi
		
		SUBROUTINE FormulationTerms_set_linFiFunction(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%linFiFunction = val
		END SUBROUTINE FormulationTerms_set_linFiFunction
		
		FUNCTION FormulationTerms_get_linFiFunction(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_linFiFunction
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_linFiFunction = data%linFiFunction
		END FUNCTION FormulationTerms_get_linFiFunction
		
		SUBROUTINE FormulationTerms_set_linBoundFiFunction(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%linBoundFiFunction = val
		END SUBROUTINE FormulationTerms_set_linBoundFiFunction
		
		FUNCTION FormulationTerms_get_linBoundFiFunction(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_linBoundFiFunction
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_linBoundFiFunction = data%linBoundFiFunction
		END FUNCTION FormulationTerms_get_linBoundFiFunction
		
		SUBROUTINE FormulationTerms_set_bilBoundFiFi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilBoundFiFi = val
		END SUBROUTINE FormulationTerms_set_bilBoundFiFi
		
		FUNCTION FormulationTerms_get_bilBoundFiFi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_bilBoundFiFi
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_bilBoundFiFi = data%bilBoundFiFi
		END FUNCTION FormulationTerms_get_bilBoundFiFi
		
		SUBROUTINE FormulationTerms_set_bilRotNiRotNi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilRotNiRotNi = val
		END SUBROUTINE FormulationTerms_set_bilRotNiRotNi
		
		FUNCTION FormulationTerms_get_bilRotNiRotNi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_bilRotNiRotNi
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_bilRotNiRotNi = data%bilRotNiRotNi
		END FUNCTION FormulationTerms_get_bilRotNiRotNi
		
		SUBROUTINE FormulationTerms_set_bilNiNi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilNiNi = val
		END SUBROUTINE FormulationTerms_set_bilNiNi
		
		FUNCTION FormulationTerms_get_bilNiNi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_bilNiNi
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_bilNiNi = data%bilNiNi
		END FUNCTION FormulationTerms_get_bilNiNi
		
		SUBROUTINE FormulationTerms_set_linNiFunction(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%linNiFunction = val
		END SUBROUTINE FormulationTerms_set_linNiFunction
		
		FUNCTION FormulationTerms_get_linNiFunction(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_linNiFunction
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_linNiFunction = data%linNiFunction
		END FUNCTION FormulationTerms_get_linNiFunction
		
		SUBROUTINE FormulationTerms_set_linBoundNiFunction(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%linBoundNiFunction = val
		END SUBROUTINE FormulationTerms_set_linBoundNiFunction
		
		FUNCTION FormulationTerms_get_linBoundNiFunction(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_linBoundNiFunction
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_linBoundNiFunction = data%linBoundNiFunction
		END FUNCTION FormulationTerms_get_linBoundNiFunction
		
		SUBROUTINE FormulationTerms_set_bilBoundNiNi(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL), VALUE :: val
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%bilBoundNiNi = val
		END SUBROUTINE FormulationTerms_set_bilBoundNiNi
		
		FUNCTION FormulationTerms_get_bilBoundNiNi(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			LOGICAL(C_BOOL) :: FormulationTerms_get_bilBoundNiNi
			TYPE(FormulationTerms), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			FormulationTerms_get_bilBoundNiNi = data%bilBoundNiNi
		END FUNCTION FormulationTerms_get_bilBoundNiNi
		
		SUBROUTINE FormulationTerms_set_variational_unknown(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(FormulationTerms), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%variational_unknown(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%variational_unknown(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE FormulationTerms_set_variational_unknown
		
		SUBROUTINE FormulationTerms_get_variational_unknown(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(FormulationTerms), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%variational_unknown(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%variational_unknown(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE FormulationTerms_get_variational_unknown
		
		SUBROUTINE FormulationTerms_set_basis_functions(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(FormulationTerms), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%basis_functions(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%basis_functions(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE FormulationTerms_set_basis_functions
		
		SUBROUTINE FormulationTerms_get_basis_functions(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(FormulationTerms), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%basis_functions(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%basis_functions(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE FormulationTerms_get_basis_functions
		
		SUBROUTINE FormulationTerms_set_physics(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(FormulationTerms), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%physics(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%physics(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE FormulationTerms_set_physics
		
		SUBROUTINE FormulationTerms_get_physics(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(FormulationTerms), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%physics(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%physics(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE FormulationTerms_get_physics
		
		SUBROUTINE print_FormulationTerms(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(FormulationTerms), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "FormulationTerms"
			PRINT *, "bilGradFiGradFi:", data%bilGradFiGradFi
			PRINT *, "bilFiFi:", data%bilFiFi
			PRINT *, "linFiFunction:", data%linFiFunction
			PRINT *, "linBoundFiFunction:", data%linBoundFiFunction
			PRINT *, "bilBoundFiFi:", data%bilBoundFiFi
			PRINT *, "bilRotNiRotNi:", data%bilRotNiRotNi
			PRINT *, "bilNiNi:", data%bilNiNi
			PRINT *, "linNiFunction:", data%linNiFunction
			PRINT *, "linBoundNiFunction:", data%linBoundNiFunction
			PRINT *, "bilBoundNiNi:", data%bilBoundNiNi
			PRINT *, "variational_unknown:", data%variational_unknown
			PRINT *, "basis_functions:", data%basis_functions
			PRINT *, "physics:", data%physics
		
		END SUBROUTINE print_FormulationTerms
		
		FUNCTION new_FormulationTerms() BIND(C)
			TYPE(C_PTR) :: new_FormulationTerms
			TYPE(FormulationTerms), POINTER :: obj
		
			ALLOCATE(obj)
			new_FormulationTerms = c_loc(obj)
		END FUNCTION new_FormulationTerms
		
		SUBROUTINE free_FormulationTerms(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(FormulationTerms), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_FormulationTerms
		
		FUNCTION get_thermal_simulation() BIND(C)
			TYPE(C_PTR) :: get_thermal_simulation
			TYPE(thermal_formulation), POINTER :: ptr
			ptr => thermal_simulation
			get_thermal_simulation = c_loc(ptr)
		END FUNCTION get_thermal_simulation
		
		FUNCTION get_electromagnetic_simulation() BIND(C)
			TYPE(C_PTR) :: get_electromagnetic_simulation
			TYPE(double_curl_wave_formulation), POINTER :: ptr
			ptr => electromagnetic_simulation
			get_electromagnetic_simulation = c_loc(ptr)
		END FUNCTION get_electromagnetic_simulation
		
		FUNCTION get_formulation_constants() BIND(C)
			TYPE(C_PTR) :: get_formulation_constants
			TYPE(FormulationConstants), POINTER :: ptr
			ptr => formulation_constants
			get_formulation_constants = c_loc(ptr)
		END FUNCTION get_formulation_constants
		
		FUNCTION get_formulation_terms() BIND(C)
			TYPE(C_PTR) :: get_formulation_terms
			TYPE(FormulationTerms), POINTER :: ptr
			ptr => formulation_terms
			get_formulation_terms = c_loc(ptr)
		END FUNCTION get_formulation_terms
		


!@C Anything inside this section will be preserved by the builder

!/@C

END MODULE formulation_terms_module_jl_interface
