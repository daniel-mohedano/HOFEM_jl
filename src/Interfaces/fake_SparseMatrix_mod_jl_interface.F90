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
!> Automatically generated Julia interface module for fake_SparseMatrix_mod_jl_interface
!----------------------------------------------------------------------------------------------------------------------

MODULE fake_SparseMatrix_mod_jl_interface
	USE iso_c_binding
	USE fake_SparseMatrix_mod
	
	IMPLICIT NONE
	
	CONTAINS
!@C Anything inside this section will be preserved by the builder

!/@C

		FUNCTION new_SparseMatrix_type() BIND(C)
			TYPE(C_PTR) :: new_SparseMatrix_type
			TYPE(SparseMatrix_type), POINTER :: obj
		
			ALLOCATE(obj)
			new_SparseMatrix_type = c_loc(obj)
		END FUNCTION new_SparseMatrix_type
		
		SUBROUTINE free_SparseMatrix_type(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(SparseMatrix_type), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_SparseMatrix_type
		
		SUBROUTINE SparseMatrix_type_set_n(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%n = val
		END SUBROUTINE SparseMatrix_type_set_n
		
		FUNCTION SparseMatrix_type_get_n(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: SparseMatrix_type_get_n
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			SparseMatrix_type_get_n = data%n
		END FUNCTION SparseMatrix_type_get_n
		
		SUBROUTINE SparseMatrix_type_set_nnz(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%nnz = val
		END SUBROUTINE SparseMatrix_type_set_nnz
		
		FUNCTION SparseMatrix_type_get_nnz(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: SparseMatrix_type_get_nnz
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			SparseMatrix_type_get_nnz = data%nnz
		END FUNCTION SparseMatrix_type_get_nnz
		
		SUBROUTINE SparseMatrix_type_set_mat_type_format(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%mat_type_format = val
		END SUBROUTINE SparseMatrix_type_set_mat_type_format
		
		FUNCTION SparseMatrix_type_get_mat_type_format(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: SparseMatrix_type_get_mat_type_format
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			SparseMatrix_type_get_mat_type_format = data%mat_type_format
		END FUNCTION SparseMatrix_type_get_mat_type_format
		
		SUBROUTINE SparseMatrix_type_set_flag_sym(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%flag_sym = val
		END SUBROUTINE SparseMatrix_type_set_flag_sym
		
		FUNCTION SparseMatrix_type_get_flag_sym(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: SparseMatrix_type_get_flag_sym
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			SparseMatrix_type_get_flag_sym = data%flag_sym
		END FUNCTION SparseMatrix_type_get_flag_sym
		
		SUBROUTINE SparseMatrix_type_set_flag_complex(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%flag_complex = val
		END SUBROUTINE SparseMatrix_type_set_flag_complex
		
		FUNCTION SparseMatrix_type_get_flag_complex(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: SparseMatrix_type_get_flag_complex
			TYPE(SparseMatrix_type), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			SparseMatrix_type_get_flag_complex = data%flag_complex
		END FUNCTION SparseMatrix_type_get_flag_complex
		
		FUNCTION new_SparseMatrix_type_real() BIND(C)
			TYPE(C_PTR) :: new_SparseMatrix_type_real
			TYPE(SparseMatrix_type_real), POINTER :: obj
		
			ALLOCATE(obj)
			new_SparseMatrix_type_real = c_loc(obj)
		END FUNCTION new_SparseMatrix_type_real
		
		SUBROUTINE free_SparseMatrix_type_real(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(SparseMatrix_type_real), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_SparseMatrix_type_real
		
		SUBROUTINE print_SparseMatrix_type_real(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(SparseMatrix_type_real), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "SparseMatrix_type_real"
			PRINT *, "A:", data%A
		END SUBROUTINE print_SparseMatrix_type_real
		
		FUNCTION new_SparseMatrix_type_complex() BIND(C)
			TYPE(C_PTR) :: new_SparseMatrix_type_complex
			TYPE(SparseMatrix_type_complex), POINTER :: obj
		
			ALLOCATE(obj)
			new_SparseMatrix_type_complex = c_loc(obj)
		END FUNCTION new_SparseMatrix_type_complex
		
		SUBROUTINE free_SparseMatrix_type_complex(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(SparseMatrix_type_complex), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_SparseMatrix_type_complex
		
		SUBROUTINE print_SparseMatrix_type_complex(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(SparseMatrix_type_complex), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "SparseMatrix_type_complex"
			PRINT *, "A:", data%A
		END SUBROUTINE print_SparseMatrix_type_complex
		
		SUBROUTINE jl_print_SparseMatrix_type(this) BIND(C)
			TYPE(C_PTR), VALUE :: this
			TYPE(SparseMatrix_type), POINTER :: this_f
			CALL c_f_pointer(this, this_f)
			
			CALL print_SparseMatrix_type(this_f)
		END SUBROUTINE jl_print_SparseMatrix_type
		
		SUBROUTINE jl_deallocate_SparseMatrix_type_components(this) BIND(C)
			TYPE(C_PTR), VALUE :: this
			TYPE(SparseMatrix_type), POINTER :: this_f
			CALL c_f_pointer(this, this_f)
			
			CALL deallocate_SparseMatrix_type_components(this_f)
		END SUBROUTINE jl_deallocate_SparseMatrix_type_components
		
		SUBROUTINE jl_set_default_SparseMatrix_type(FEM_matrix) BIND(C)
			TYPE(C_PTR), VALUE :: FEM_matrix
			TYPE(SparseMatrix_type), POINTER :: FEM_matrix_f
			CALL c_f_pointer(FEM_matrix, FEM_matrix_f)
			
			CALL set_default_SparseMatrix_type(FEM_matrix_f)
		END SUBROUTINE jl_set_default_SparseMatrix_type
		
		SUBROUTINE jl_set_default_DenseRHS_type(DenseRHS) BIND(C)
			TYPE(C_PTR), VALUE :: DenseRHS
			TYPE(SparseMatrix_type), POINTER :: DenseRHS_f
			CALL c_f_pointer(DenseRHS, DenseRHS_f)
			
			CALL set_default_DenseRHS_type(DenseRHS_f)
		END SUBROUTINE jl_set_default_DenseRHS_type
		
		SUBROUTINE jl_set_SparseMatrix_type(FEM_matrix, n, mat_type_format, flag_sym, IRN_size, IRN, JCN_size, JCN) BIND(C)
			TYPE(C_PTR), VALUE :: FEM_matrix
			TYPE(SparseMatrix_type), POINTER :: FEM_matrix_f
			INTEGER(C_INT), VALUE :: n
			INTEGER(C_INT), VALUE :: mat_type_format
			INTEGER(C_INT), VALUE :: flag_sym
			INTEGER, VALUE :: IRN_size
			TYPE(C_PTR), VALUE :: IRN
			INTEGER(C_INT), POINTER :: IRN_f(:)
			INTEGER, VALUE :: JCN_size
			TYPE(C_PTR), VALUE :: JCN
			INTEGER(C_INT), POINTER :: JCN_f(:)
			CALL c_f_pointer(FEM_matrix, FEM_matrix_f)
			CALL c_f_pointer(IRN, IRN_f, [IRN_size])
			CALL c_f_pointer(JCN, JCN_f, [JCN_size])
			
			CALL set_SparseMatrix_type(FEM_matrix_f, n, mat_type_format, flag_sym, IRN_f, JCN_f)
		END SUBROUTINE jl_set_SparseMatrix_type
		
		SUBROUTINE jl_set_DenseRHS_type(DenseRHS, n, mat_type_format, flag_sym) BIND(C)
			TYPE(C_PTR), VALUE :: DenseRHS
			TYPE(SparseMatrix_type), POINTER :: DenseRHS_f
			INTEGER(C_INT), VALUE :: n
			INTEGER(C_INT), VALUE :: mat_type_format
			INTEGER(C_INT), VALUE :: flag_sym
			CALL c_f_pointer(DenseRHS, DenseRHS_f)
			
			CALL set_DenseRHS_type(DenseRHS_f, n, mat_type_format, flag_sym)
		END SUBROUTINE jl_set_DenseRHS_type
		

END MODULE fake_SparseMatrix_mod_jl_interface
