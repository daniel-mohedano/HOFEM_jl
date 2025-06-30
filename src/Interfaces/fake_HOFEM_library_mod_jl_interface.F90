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
!> Automatically generated Julia interface module for fake_HOFEM_library_mod_jl_interface
!----------------------------------------------------------------------------------------------------------------------
MODULE fake_HOFEM_library_mod_jl_interface
	USE iso_c_binding
	USE fake_HOFEM_library_mod
	
	IMPLICIT NONE
	
	CONTAINS
!@C Anything inside this section will be preserved by the builder

!/@C

		FUNCTION new_fakeObject_t() BIND(C)
			TYPE(C_PTR) :: new_fakeObject_t
			TYPE(fakeObject_t), POINTER :: obj
		
			ALLOCATE(obj)
			new_fakeObject_t = c_loc(obj)
		END FUNCTION new_fakeObject_t
		
		SUBROUTINE free_fakeObject_t(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(fakeObject_t), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_fakeObject_t
		
		SUBROUTINE fakeObject_t_set_myinteger(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT), VALUE :: val
			TYPE(fakeObject_t), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			data%myinteger = val
		END SUBROUTINE fakeObject_t_set_myinteger
		
		FUNCTION fakeObject_t_get_myinteger(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			INTEGER(C_INT) :: fakeObject_t_get_myinteger
			TYPE(fakeObject_t), POINTER :: data
			
			CALL c_f_pointer(data_c_ptr, data)
			fakeObject_t_get_myinteger = data%myinteger
		END FUNCTION fakeObject_t_get_myinteger
		
		SUBROUTINE fakeObject_t_set_mymessage(data_c_ptr, val) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: val
			TYPE(fakeObject_t), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((val(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
				data%mymessage(i:i) = val(i)
				i = i + 1
			END DO
			DO WHILE (i <= LENGTH)
				data%mymessage(i:i) = " "
				i = i + 1
			END DO
		END SUBROUTINE fakeObject_t_set_mymessage
		
		SUBROUTINE fakeObject_t_get_mymessage(data_c_ptr, string) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: string
			TYPE(fakeObject_t), POINTER :: data
			INTEGER :: i = 1
			
			CALL c_f_pointer(data_c_ptr, data)
			DO
				IF ((data%mymessage(i:i) == " ") .OR. (i == LENGTH)) EXIT
				string(i) = data%mymessage(i:i)
				i = i + 1
			END DO
			string(i) = C_NULL_CHAR
		END SUBROUTINE fakeObject_t_get_mymessage
		
		SUBROUTINE print_fakeObject_t(data_c_ptr) BIND(C)
			TYPE(C_PTR), VALUE :: data_c_ptr
			TYPE(fakeObject_t), POINTER :: data
			CALL c_f_pointer(data_c_ptr, data)
			PRINT *, "fakeObject_t"
			PRINT *, "myinteger:", data%myinteger
			PRINT *, "mymessage:", data%mymessage
		END SUBROUTINE print_fakeObject_t
		
		SUBROUTINE jl_print_fakeObject(thisfakeObject) BIND(C)
			TYPE(C_PTR), VALUE :: thisfakeObject
			TYPE(fakeObject_t), POINTER :: thisfakeObject_f
			CALL c_f_pointer(thisfakeObject, thisfakeObject_f)
			
			CALL print_fakeObject(thisfakeObject_f)
		END SUBROUTINE jl_print_fakeObject
		

END MODULE fake_HOFEM_library_mod_jl_interface
