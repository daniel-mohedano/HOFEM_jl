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
!> Automatically generated Julia interface module for mesh_object_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------

MODULE mesh_object_module_jl_interface
	USE iso_c_binding
	USE mesh_object_module
	
	IMPLICIT NONE
	
	CONTAINS
!@C Anything inside this section will be preserved by the builder

!/@C

		FUNCTION new_MeshObject() BIND(C)
			TYPE(C_PTR) :: new_MeshObject
			TYPE(MeshObject), POINTER :: obj
		
			ALLOCATE(obj)
			new_MeshObject = c_loc(obj)
		END FUNCTION new_MeshObject
		
		SUBROUTINE free_MeshObject(p) BIND(C)
			TYPE(C_PTR), VALUE :: p
			TYPE(MeshObject), POINTER :: obj
		
			CALL c_f_pointer(p, obj)
			IF (ASSOCIATED(obj)) DEALLOCATE(obj)
		END SUBROUTINE free_MeshObject
		
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
			PRINT *, "tetrahedras:", data%tetrahedras
			PRINT *, "adaptetras:", data%adaptetras
			PRINT *, "number_of_prisms:", data%number_of_prisms
			PRINT *, "prisms:", data%prisms
			PRINT *, "number_of_hexahedras:", data%number_of_hexahedras
			PRINT *, "mesh_partition:", data%mesh_partition
			PRINT *, "lower_dof_id:", data%lower_dof_id
			PRINT *, "upper_dof_id:", data%upper_dof_id
		END SUBROUTINE print_MeshObject
		
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
		
		FUNCTION jl_get_total_number_of_dofs(mesh) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_total_number_of_dofs
			
			jl_get_total_number_of_dofs = get_total_number_of_dofs(mesh_f)
		END FUNCTION jl_get_total_number_of_dofs
		
		FUNCTION jl_get_total_number_of_elements(mesh) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_total_number_of_elements
			
			jl_get_total_number_of_elements = get_total_number_of_elements(mesh_f)
		END FUNCTION jl_get_total_number_of_elements
		
		FUNCTION jl_get_total_number_of_points(mesh) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_total_number_of_points
			
			jl_get_total_number_of_points = get_total_number_of_points(mesh_f)
		END FUNCTION jl_get_total_number_of_points
		
		FUNCTION jl_get_mesh_geometrical_order(mesh) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_mesh_geometrical_order
			
			jl_get_mesh_geometrical_order = get_mesh_geometrical_order(mesh_f)
		END FUNCTION jl_get_mesh_geometrical_order
		
		FUNCTION jl_get_mesh_variational_order(mesh) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_mesh_variational_order
			
			jl_get_mesh_variational_order = get_mesh_variational_order(mesh_f)
		END FUNCTION jl_get_mesh_variational_order
		
		FUNCTION jl_is_this_element_filling_rhs(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			LOGICAL(C_BOOL) :: jl_is_this_element_filling_rhs
			
			jl_is_this_element_filling_rhs = is_this_element_filling_rhs(mesh_f, elemID)
		END FUNCTION jl_is_this_element_filling_rhs
		
		FUNCTION jl_get_topology_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			CHARACTER(KIND=C_CHAR), DIMENSION(*) :: jl_get_topology_of_this_element
			
			jl_get_topology_of_this_element = get_topology_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_topology_of_this_element
		
		FUNCTION jl_get_material_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_material_of_this_element
			
			jl_get_material_of_this_element = get_material_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_material_of_this_element
		
		FUNCTION jl_get_curved_flag_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			LOGICAL(C_BOOL) :: jl_get_curved_flag_of_this_element
			
			jl_get_curved_flag_of_this_element = get_curved_flag_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_curved_flag_of_this_element
		
		FUNCTION jl_get_surfacesets_IDS_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_surfacesets_IDS_of_this_element
			
			jl_get_surfacesets_IDS_of_this_element = get_surfacesets_IDS_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_surfacesets_IDS_of_this_element
		
		FUNCTION jl_get_faces_over_interior_region_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			LOGICAL(C_BOOL) :: jl_get_faces_over_interior_region_of_this_element
			
			jl_get_faces_over_interior_region_of_this_element = get_faces_over_interior_region_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_faces_over_interior_region_of_this_element
		
		FUNCTION jl_get_boundary_condition_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_boundary_condition_of_this_element
			
			jl_get_boundary_condition_of_this_element = get_boundary_condition_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_boundary_condition_of_this_element
		
		SUBROUTINE jl_get_nodes_connectivity_of_this_element(mesh, elemID, nodes, npe) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(C_PTR), VALUE :: nodes
			INTEGER(C_INT), VALUE :: npe
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT), POINTER :: nodes_f(:)
			CALL c_f_pointer(nodes, nodes_f, [size])
			
			CALL get_nodes_connectivity_of_this_element(mesh_f, elemID, nodes_f, npe)
		END SUBROUTINE jl_get_nodes_connectivity_of_this_element
		
		SUBROUTINE jl_get_points_connectivity_of_this_element(mesh, elemID, points, ppe) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(C_PTR), VALUE :: points
			INTEGER(C_INT), VALUE :: ppe
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT), POINTER :: points_f(:)
			CALL c_f_pointer(points, points_f, [size])
			
			CALL get_points_connectivity_of_this_element(mesh_f, elemID, points_f, ppe)
		END SUBROUTINE jl_get_points_connectivity_of_this_element
		
		FUNCTION jl_get_variational_order_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_variational_order_of_this_element
			
			jl_get_variational_order_of_this_element = get_variational_order_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_variational_order_of_this_element
		
		FUNCTION jl_get_geometrical_order_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_geometrical_order_of_this_element
			
			jl_get_geometrical_order_of_this_element = get_geometrical_order_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_geometrical_order_of_this_element
		
		FUNCTION jl_get_number_of_nodes_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_number_of_nodes_of_this_element
			
			jl_get_number_of_nodes_of_this_element = get_number_of_nodes_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_number_of_nodes_of_this_element
		
		FUNCTION jl_get_number_of_points_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_number_of_points_of_this_element
			
			jl_get_number_of_points_of_this_element = get_number_of_points_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_number_of_points_of_this_element
		
		FUNCTION jl_get_all_local_points_IDS_of_this_edge(mesh, elemID, edge) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			INTEGER(C_INT), VALUE :: edge
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_all_local_points_IDS_of_this_edge
			
			jl_get_all_local_points_IDS_of_this_edge = get_all_local_points_IDS_of_this_edge(mesh_f, elemID, edge)
		END FUNCTION jl_get_all_local_points_IDS_of_this_edge
		
		FUNCTION jl_get_all_local_points_IDS_of_this_face(mesh, elemID, face) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			INTEGER(C_INT), VALUE :: face
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_all_local_points_IDS_of_this_face
			
			jl_get_all_local_points_IDS_of_this_face = get_all_local_points_IDS_of_this_face(mesh_f, elemID, face)
		END FUNCTION jl_get_all_local_points_IDS_of_this_face
		
		FUNCTION jl_get_local_points_with_fake_IDS_of_this_face(mesh, elemID, face) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			INTEGER(C_INT), VALUE :: face
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_local_points_with_fake_IDS_of_this_face
			
			jl_get_local_points_with_fake_IDS_of_this_face = get_local_points_with_fake_IDS_of_this_face(mesh_f, elemID, face)
		END FUNCTION jl_get_local_points_with_fake_IDS_of_this_face
		
		FUNCTION jl_get_all_local_nodes_IDS_of_this_edge(mesh, elemID, edge) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			INTEGER(C_INT), VALUE :: edge
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_all_local_nodes_IDS_of_this_edge
			
			jl_get_all_local_nodes_IDS_of_this_edge = get_all_local_nodes_IDS_of_this_edge(mesh_f, elemID, edge)
		END FUNCTION jl_get_all_local_nodes_IDS_of_this_edge
		
		FUNCTION jl_get_all_local_nodes_IDS_of_this_face(mesh, elemID, face) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			INTEGER(C_INT), VALUE :: face
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_all_local_nodes_IDS_of_this_face
			
			jl_get_all_local_nodes_IDS_of_this_face = get_all_local_nodes_IDS_of_this_face(mesh_f, elemID, face)
		END FUNCTION jl_get_all_local_nodes_IDS_of_this_face
		
		FUNCTION jl_get_lagrange_local_nodes_of_this_face(mesh, elemID, face) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			INTEGER(C_INT), VALUE :: face
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_lagrange_local_nodes_of_this_face
			
			jl_get_lagrange_local_nodes_of_this_face = get_lagrange_local_nodes_of_this_face(mesh_f, elemID, face)
		END FUNCTION jl_get_lagrange_local_nodes_of_this_face
		
		FUNCTION jl_get_number_of_faces_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_number_of_faces_of_this_element
			
			jl_get_number_of_faces_of_this_element = get_number_of_faces_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_number_of_faces_of_this_element
		
		FUNCTION jl_get_number_of_edges_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_number_of_edges_of_this_element
			
			jl_get_number_of_edges_of_this_element = get_number_of_edges_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_number_of_edges_of_this_element
		
		FUNCTION jl_get_number_of_vertices_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_number_of_vertices_of_this_element
			
			jl_get_number_of_vertices_of_this_element = get_number_of_vertices_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_number_of_vertices_of_this_element
		
		FUNCTION jl_get_dof_per_volume_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_dof_per_volume_of_this_element
			
			jl_get_dof_per_volume_of_this_element = get_dof_per_volume_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_dof_per_volume_of_this_element
		
		FUNCTION jl_get_dof_per_face_of_this_element(mesh, elemID, face) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			INTEGER(C_INT), VALUE :: face
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_dof_per_face_of_this_element
			
			jl_get_dof_per_face_of_this_element = get_dof_per_face_of_this_element(mesh_f, elemID, face)
		END FUNCTION jl_get_dof_per_face_of_this_element
		
		FUNCTION jl_get_dof_per_edge_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_dof_per_edge_of_this_element
			
			jl_get_dof_per_edge_of_this_element = get_dof_per_edge_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_dof_per_edge_of_this_element
		
		FUNCTION jl_get_dof_per_vertex_of_this_element(mesh, elemID) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			INTEGER(C_INT), VALUE :: elemID
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			INTEGER(C_INT) :: jl_get_dof_per_vertex_of_this_element
			
			jl_get_dof_per_vertex_of_this_element = get_dof_per_vertex_of_this_element(mesh_f, elemID)
		END FUNCTION jl_get_dof_per_vertex_of_this_element
		
		SUBROUTINE jl_create_element_type_objects_in_mesh(mesh) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			
			CALL create_element_type_objects_in_mesh(mesh_f)
		END SUBROUTINE jl_create_element_type_objects_in_mesh
		
		SUBROUTINE jl_deallocate_mesh_array_memory(mesh) BIND(C)
			TYPE(C_PTR), VALUE :: mesh
			TYPE(MeshObject), POINTER :: mesh_f
			CALL c_f_pointer(mesh, mesh_f)
			
			CALL deallocate_mesh_array_memory(mesh_f)
		END SUBROUTINE jl_deallocate_mesh_array_memory
		

END MODULE mesh_object_module_jl_interface
