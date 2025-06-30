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
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for postprocess_coefficients_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------

MODULE postprocess_coefficients_module_jl_interface
	USE iso_c_binding
	USE postprocess_coefficients_module
	
	IMPLICIT NONE
	
	CONTAINS
!@C Anything inside this section will be preserved by the builder

!/@C

		SUBROUTINE jl_write_thermal_coefficients_into_postprocess_file(mesh, time_step) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			REAL(C_DOUBLE), VALUE :: time_step
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			
			CALL write_thermal_coefficients_into_postprocess_file(mesh_f, time_step)
		END SUBROUTINE jl_write_thermal_coefficients_into_postprocess_file
		
		SUBROUTINE jl_write_coefficients_into_postprocess_file(mesh, frequency, rhs_lower, rhs_upper) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: frequency
			INTEGER(C_INT), VALUE :: rhs_lower
			INTEGER(C_INT), VALUE :: rhs_upper
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			
			CALL write_coefficients_into_postprocess_file(mesh_f, frequency, rhs_lower, rhs_upper)
		END SUBROUTINE jl_write_coefficients_into_postprocess_file
		
		SUBROUTINE jl_read_coefficients_from_postprocess_file(frequency, rhs, coeffs) BIND(C)
			INTEGER(C_INT), VALUE :: frequency
			INTEGER(C_INT), VALUE :: rhs
			TYPE(C_PTR), VALUE :: coeffs
			COMPLEX(C_DOUBLE_COMPLEX), POINTER :: coeffs_f(:)
			CALL c_f_pointer(coeffs, coeffs_f, [size])
			
			CALL read_coefficients_from_postprocess_file(frequency, rhs, coeffs_f)
		END SUBROUTINE jl_read_coefficients_from_postprocess_file
		
		SUBROUTINE jl_postprocess_get_gid_gauss_points(mesh, gauss_points) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			TYPE(C_PTR), VALUE :: gauss_points
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			REAL(C_DOUBLE), POINTER :: gauss_points_f(:)
			CALL c_f_pointer(gauss_points, gauss_points_f, [size])
			
			CALL postprocess_get_gid_gauss_points(mesh_f, gauss_points_f)
		END SUBROUTINE jl_postprocess_get_gid_gauss_points
		

END MODULE postprocess_coefficients_module_jl_interface
