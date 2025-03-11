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
!> Automatically generated Julia interface module for mesh_object_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
MODULE mesh_object_module_jl_interface
    USE iso_c_binding
    USE mesh_object_module

    IMPLICIT NONE

    CONTAINS
        SUBROUTINE MeshObject_set_geo_order(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%geo_order = val
        END SUBROUTINE MeshObject_set_geo_order
    
        FUNCTION MeshObject_get_geo_order(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_geo_order
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_geo_order = data%geo_order
        END FUNCTION MeshObject_get_geo_order
    
        SUBROUTINE MeshObject_set_var_order(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%var_order = val
        END SUBROUTINE MeshObject_set_var_order
    
        FUNCTION MeshObject_get_var_order(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_var_order
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_var_order = data%var_order
        END FUNCTION MeshObject_get_var_order
    
        SUBROUTINE MeshObject_set_num_dofs(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%num_dofs = val
        END SUBROUTINE MeshObject_set_num_dofs
    
        FUNCTION MeshObject_get_num_dofs(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_num_dofs
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_num_dofs = data%num_dofs
        END FUNCTION MeshObject_get_num_dofs
    
        SUBROUTINE MeshObject_set_number_of_points(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%number_of_points = val
        END SUBROUTINE MeshObject_set_number_of_points
    
        FUNCTION MeshObject_get_number_of_points(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_number_of_points
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_number_of_points = data%number_of_points
        END FUNCTION MeshObject_get_number_of_points
    
        SUBROUTINE MeshObject_set_number_of_tetrahedras(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%number_of_tetrahedras = val
        END SUBROUTINE MeshObject_set_number_of_tetrahedras
    
        FUNCTION MeshObject_get_number_of_tetrahedras(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_number_of_tetrahedras
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_number_of_tetrahedras = data%number_of_tetrahedras
        END FUNCTION MeshObject_get_number_of_tetrahedras
    
        SUBROUTINE MeshObject_set_number_of_prisms(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%number_of_prisms = val
        END SUBROUTINE MeshObject_set_number_of_prisms
    
        FUNCTION MeshObject_get_number_of_prisms(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_number_of_prisms
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_number_of_prisms = data%number_of_prisms
        END FUNCTION MeshObject_get_number_of_prisms
    
        SUBROUTINE MeshObject_set_number_of_hexahedras(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%number_of_hexahedras = val
        END SUBROUTINE MeshObject_set_number_of_hexahedras
    
        FUNCTION MeshObject_get_number_of_hexahedras(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_number_of_hexahedras
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_number_of_hexahedras = data%number_of_hexahedras
        END FUNCTION MeshObject_get_number_of_hexahedras
    
        SUBROUTINE MeshObject_set_lower_dof_id(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%lower_dof_id = val
        END SUBROUTINE MeshObject_set_lower_dof_id
    
        FUNCTION MeshObject_get_lower_dof_id(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_lower_dof_id
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_lower_dof_id = data%lower_dof_id
        END FUNCTION MeshObject_get_lower_dof_id
    
        SUBROUTINE MeshObject_set_upper_dof_id(data_c_ptr, val) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT), VALUE :: val
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            data%upper_dof_id = val
        END SUBROUTINE MeshObject_set_upper_dof_id
    
        FUNCTION MeshObject_get_upper_dof_id(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            INTEGER(C_INT) :: MeshObject_get_upper_dof_id
            TYPE(MeshObject), POINTER :: data
            
            CALL c_f_pointer(data_c_ptr, data)
            MeshObject_get_upper_dof_id = data%upper_dof_id
        END FUNCTION MeshObject_get_upper_dof_id
    
        FUNCTION get_local_mesh() BIND(C)
            TYPE(C_PTR) :: get_local_mesh
            TYPE(MeshObject), POINTER :: ptr
            ptr => local_mesh
            get_local_mesh = c_loc(ptr)
        END FUNCTION get_local_mesh
    
        FUNCTION get_global_mesh() BIND(C)
            TYPE(C_PTR) :: get_global_mesh
            TYPE(MeshObject), POINTER :: ptr
            ptr => global_mesh
            get_global_mesh = c_loc(ptr)
        END FUNCTION get_global_mesh
    
        FUNCTION get_IIEE_mesh() BIND(C)
            TYPE(C_PTR) :: get_IIEE_mesh
            TYPE(MeshObject), POINTER :: ptr
            ptr => IIEE_mesh
            get_IIEE_mesh = c_loc(ptr)
        END FUNCTION get_IIEE_mesh
    
        SUBROUTINE print_MeshObject(data_c_ptr) BIND(C)
            TYPE(C_PTR), VALUE :: data_c_ptr
            TYPE(MeshObject), POINTER :: data
            CALL c_f_pointer(data_c_ptr, data)
            PRINT *, "MeshObject"
            PRINT *, "geo_order:", data%geo_order
            PRINT *, "var_order:", data%var_order
            PRINT *, "num_dofs:", data%num_dofs
            PRINT *, "number_of_points:", data%number_of_points
            PRINT *, "number_of_tetrahedras:", data%number_of_tetrahedras
            PRINT *, "number_of_prisms:", data%number_of_prisms
            PRINT *, "number_of_hexahedras:", data%number_of_hexahedras
            PRINT *, "lower_dof_id:", data%lower_dof_id
            PRINT *, "upper_dof_id:", data%upper_dof_id
        
        END SUBROUTINE print_MeshObject
    


!@C Anything inside this section will be preserved by the builder

!/@C

END MODULE mesh_object_module_jl_interface
