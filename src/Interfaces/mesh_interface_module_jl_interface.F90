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
!> Automatically generated Julia interface module for mesh_interface_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
MODULE mesh_interface_module_jl_interface
	USE iso_c_binding
	USE mesh_interface_module
	
	IMPLICIT NONE
	
	CONTAINS
!@C Anything inside this section will be preserved by the builder
        SUBROUTINE jl_mesh_interface_constructor(filename, mesh_ptr) BIND(C)
            CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
            TYPE(C_PTR), VALUE :: mesh_ptr
            CHARACTER(LEN=LENGTH) :: meshfilename
            TYPE(MeshObject), POINTER :: mesh
            INTEGER :: i = 1

            ! Reconstruct arguments
            CALL c_f_pointer(mesh_ptr, mesh)
            DO
                IF ((filename(i) == C_NULL_CHAR) .OR. (i == LENGTH)) EXIT
                meshfilename(i:i) = filename(i)
                i = i + 1
            END DO
            DO WHILE (i <= LENGTH)
                meshfilename(i:i) = " "
                i = i + 1
            END DO
            
            ! Call routine
            CALL mesh_interface_constructor(meshfilename, mesh)
        END SUBROUTINE jl_mesh_interface_constructor
!/@C



END MODULE mesh_interface_module_jl_interface
