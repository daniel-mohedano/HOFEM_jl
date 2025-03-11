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
!> 7 October 2024
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
    
        FUNCTION get_thermal_simulation() BIND(C)
            TYPE(C_PTR) :: get_thermal_simulation
            TYPE(thermal_formulation), POINTER :: ptr
            ptr => thermal_simulation
            get_thermal_simulation = c_loc(ptr)
        END FUNCTION get_thermal_simulation
    
        SUBROUTINE print_thermal_formulation(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            TYPE(thermal_formulation), POINTER :: data
            CALL c_f_pointer(data_c_ptr, data)
            PRINT *, "thermal_formulation"
            PRINT *, "delta_sim_time:", data%delta_sim_time
            PRINT *, "stop_sim_time:", data%stop_sim_time
            PRINT *, "current_sim_time:", data%current_sim_time
            PRINT *, "total_time_steps:", data%total_time_steps
            PRINT *, "step_tolerance:", data%step_tolerance
            PRINT *, "time_step_strategy:", data%time_step_strategy
        
        END SUBROUTINE print_thermal_formulation
    
        FUNCTION get_electromagnetic_simulation() BIND(C)
            TYPE(C_PTR) :: get_electromagnetic_simulation
            TYPE(double_curl_wave_formulation), POINTER :: ptr
            ptr => electromagnetic_simulation
            get_electromagnetic_simulation = c_loc(ptr)
        END FUNCTION get_electromagnetic_simulation
    
        SUBROUTINE print_double_curl_wave_formulation(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            TYPE(double_curl_wave_formulation), POINTER :: data
            CALL c_f_pointer(data_c_ptr, data)
            PRINT *, "double_curl_wave_formulation"
        
        END SUBROUTINE print_double_curl_wave_formulation
    
        FUNCTION get_formulation_constants() BIND(C)
            TYPE(C_PTR) :: get_formulation_constants
            TYPE(FormulationConstants), POINTER :: ptr
            ptr => formulation_constants
            get_formulation_constants = c_loc(ptr)
        END FUNCTION get_formulation_constants
    
        SUBROUTINE print_FormulationConstants(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            TYPE(FormulationConstants), POINTER :: data
            CALL c_f_pointer(data_c_ptr, data)
            PRINT *, "FormulationConstants"
        
        END SUBROUTINE print_FormulationConstants
    
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
    
        FUNCTION get_formulation_terms() BIND(C)
            TYPE(C_PTR) :: get_formulation_terms
            TYPE(FormulationTerms), POINTER :: ptr
            ptr => formulation_terms
            get_formulation_terms = c_loc(ptr)
        END FUNCTION get_formulation_terms
    
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
    


!@C Anything inside this section will be preserved by the builder

!/@C

END MODULE formulation_terms_module_jl_interface
