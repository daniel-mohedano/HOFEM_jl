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
!> Automatically generated Julia interface module for fake_MUMPS_driver_mod_jl_interface
!----------------------------------------------------------------------------------------------------------------------

MODULE fake_MUMPS_driver_mod_jl_interface
	USE iso_c_binding
	USE fake_MUMPS_driver_mod
	
	IMPLICIT NONE
	
	CONTAINS
!@C Anything inside this section will be preserved by the builder

!/@C

		SUBROUTINE jl_fake_dMUMPS_driver_centralized(FEM_matrix_real, DenseRHS_real) BIND(C)
			TYPE(C_PTR), VALUE :: FEM_matrix_real
			TYPE(SparseMatrix_type_real), POINTER :: FEM_matrix_real_f
			TYPE(C_PTR), VALUE :: DenseRHS_real
			TYPE(SparseMatrix_type_real), POINTER :: DenseRHS_real_f
			CALL c_f_pointer(FEM_matrix_real, FEM_matrix_real_f)
			CALL c_f_pointer(DenseRHS_real, DenseRHS_real_f)
			
			CALL fake_dMUMPS_driver_centralized(FEM_matrix_real_f, DenseRHS_real_f)
		END SUBROUTINE jl_fake_dMUMPS_driver_centralized
		
		SUBROUTINE jl_fake_zMUMPS_driver_centralized(FEM_matrix_complex, DenseRHS_complex) BIND(C)
			TYPE(C_PTR), VALUE :: FEM_matrix_complex
			TYPE(SparseMatrix_type_complex), POINTER :: FEM_matrix_complex_f
			TYPE(C_PTR), VALUE :: DenseRHS_complex
			TYPE(SparseMatrix_type_complex), POINTER :: DenseRHS_complex_f
			CALL c_f_pointer(FEM_matrix_complex, FEM_matrix_complex_f)
			CALL c_f_pointer(DenseRHS_complex, DenseRHS_complex_f)
			
			CALL fake_zMUMPS_driver_centralized(FEM_matrix_complex_f, DenseRHS_complex_f)
		END SUBROUTINE jl_fake_zMUMPS_driver_centralized
		
		SUBROUTINE jl_fake_dMUMPS_driver_distributed(FEM_matrix_real, DenseRHS_real) BIND(C)
			TYPE(C_PTR), VALUE :: FEM_matrix_real
			TYPE(SparseMatrix_type_real), POINTER :: FEM_matrix_real_f
			TYPE(C_PTR), VALUE :: DenseRHS_real
			TYPE(SparseMatrix_type_real), POINTER :: DenseRHS_real_f
			CALL c_f_pointer(FEM_matrix_real, FEM_matrix_real_f)
			CALL c_f_pointer(DenseRHS_real, DenseRHS_real_f)
			
			CALL fake_dMUMPS_driver_distributed(FEM_matrix_real_f, DenseRHS_real_f)
		END SUBROUTINE jl_fake_dMUMPS_driver_distributed
		
		SUBROUTINE jl_fake_zMUMPS_driver_distributed(FEM_matrix_complex, DenseRHS_complex) BIND(C)
			TYPE(C_PTR), VALUE :: FEM_matrix_complex
			TYPE(SparseMatrix_type_complex), POINTER :: FEM_matrix_complex_f
			TYPE(C_PTR), VALUE :: DenseRHS_complex
			TYPE(SparseMatrix_type_complex), POINTER :: DenseRHS_complex_f
			CALL c_f_pointer(FEM_matrix_complex, FEM_matrix_complex_f)
			CALL c_f_pointer(DenseRHS_complex, DenseRHS_complex_f)
			
			CALL fake_zMUMPS_driver_distributed(FEM_matrix_complex_f, DenseRHS_complex_f)
		END SUBROUTINE jl_fake_zMUMPS_driver_distributed
		

END MODULE fake_MUMPS_driver_mod_jl_interface
