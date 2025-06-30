"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for mesh_object_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
#@C Anything inside this section will be preserved by the builder

#/@C

struct MeshObject
	handle::Ptr{Cvoid}
end

function MeshObject()
	return MeshObject(@ccall _HOFEM_LIB_PATH.new_meshobject()::Ptr{Cvoid})
end

function Base.finalize(obj::MeshObject)
	@ccall _HOFEM_LIB_PATH.free_meshobject(obj.handle::Ptr{Cvoid})::Cvoid
end

function set_geo_order!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_geo_order(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_geo_order(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_geo_order(obj.handle::Ptr{Cvoid})::Cint
end

function set_var_order!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_var_order(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_var_order(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_var_order(obj.handle::Ptr{Cvoid})::Cint
end

function set_num_dofs!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_num_dofs(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_num_dofs(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_num_dofs(obj.handle::Ptr{Cvoid})::Cint
end

function set_number_of_points!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_number_of_points(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_number_of_points(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_number_of_points(obj.handle::Ptr{Cvoid})::Cint
end

function set_number_of_tetrahedras!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_number_of_tetrahedras(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_number_of_tetrahedras(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_number_of_tetrahedras(obj.handle::Ptr{Cvoid})::Cint
end

function set_number_of_prisms!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_number_of_prisms(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_number_of_prisms(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_number_of_prisms(obj.handle::Ptr{Cvoid})::Cint
end

function set_number_of_hexahedras!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_number_of_hexahedras(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_number_of_hexahedras(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_number_of_hexahedras(obj.handle::Ptr{Cvoid})::Cint
end

function set_lower_dof_id!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_lower_dof_id(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_lower_dof_id(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_lower_dof_id(obj.handle::Ptr{Cvoid})::Cint
end

function set_upper_dof_id!(obj::MeshObject, val::Cint)
	@ccall _HOFEM_LIB_PATH.meshobject_set_upper_dof_id(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_upper_dof_id(obj::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.meshobject_get_upper_dof_id(obj.handle::Ptr{Cvoid})::Cint
end

function print_MeshObject(obj::MeshObject)
	@ccall _HOFEM_LIB_PATH.print_meshobject(obj.handle::Ptr{Cvoid})::Cvoid
end

function get_local_mesh()::MeshObject
	return MeshObject(@ccall _HOFEM_LIB_PATH.get_local_mesh()::Ptr{Cvoid})
end

function get_global_mesh()::MeshObject
	return MeshObject(@ccall _HOFEM_LIB_PATH.get_global_mesh()::Ptr{Cvoid})
end

function get_IIEE_mesh()::MeshObject
	return MeshObject(@ccall _HOFEM_LIB_PATH.get_iiee_mesh()::Ptr{Cvoid})
end

function get_total_number_of_dofs(mesh::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_total_number_of_dofs(mesh.handle::Ptr{Cvoid})::Cint
end

function get_total_number_of_elements(mesh::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_total_number_of_elements(mesh.handle::Ptr{Cvoid})::Cint
end

function get_total_number_of_points(mesh::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_total_number_of_points(mesh.handle::Ptr{Cvoid})::Cint
end

function get_mesh_geometrical_order(mesh::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_mesh_geometrical_order(mesh.handle::Ptr{Cvoid})::Cint
end

function get_mesh_variational_order(mesh::MeshObject)::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_mesh_variational_order(mesh.handle::Ptr{Cvoid})::Cint
end

function is_this_element_filling_rhs(mesh::MeshObject, elemID::Ref{Cint})::Cuchar
	return @ccall _HOFEM_LIB_PATH.jl_is_this_element_filling_rhs(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cuchar
end

function get_topology_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Ptr{Cchar}
	return @ccall _HOFEM_LIB_PATH.jl_get_topology_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Ptr{Cchar}
end

function get_material_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_material_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_curved_flag_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cuchar
	return @ccall _HOFEM_LIB_PATH.jl_get_curved_flag_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cuchar
end

function get_surfacesets_IDS_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Ptr{Cint}
	return @ccall _HOFEM_LIB_PATH.jl_get_surfacesets_IDS_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Ptr{Cint}
end

function get_faces_over_interior_region_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Ptr{Cuchar}
	return @ccall _HOFEM_LIB_PATH.jl_get_faces_over_interior_region_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Ptr{Cuchar}
end

function get_boundary_condition_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Ptr{Cint}
	return @ccall _HOFEM_LIB_PATH.jl_get_boundary_condition_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Ptr{Cint}
end

function get_nodes_connectivity_of_this_element(mesh::MeshObject, elemID::Ref{Cint}, nodes::Ptr{Cint}, npe::Ref{Cint})::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_get_nodes_connectivity_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, nodes::Ptr{Cint}, npe::Ref{Cint})::Cvoid
end

function get_points_connectivity_of_this_element(mesh::MeshObject, elemID::Ref{Cint}, points::Ptr{Cint}, ppe::Ref{Cint})::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_get_points_connectivity_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, points::Ptr{Cint}, ppe::Ref{Cint})::Cvoid
end

function get_variational_order_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_variational_order_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_geometrical_order_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_geometrical_order_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_number_of_nodes_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_number_of_nodes_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_number_of_points_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_number_of_points_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_all_local_points_IDS_of_this_edge(mesh::MeshObject, elemID::Ref{Cint}, edge::Ref{Cint})::Ptr{Cint}
	return @ccall _HOFEM_LIB_PATH.jl_get_all_local_points_IDS_of_this_edge(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, edge::Ref{Cint})::Ptr{Cint}
end

function get_all_local_points_IDS_of_this_face(mesh::MeshObject, elemID::Ref{Cint}, face::Ref{Cint})::Ptr{Cint}
	return @ccall _HOFEM_LIB_PATH.jl_get_all_local_points_IDS_of_this_face(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, face::Ref{Cint})::Ptr{Cint}
end

function get_local_points_with_fake_IDS_of_this_face(mesh::MeshObject, elemID::Ref{Cint}, face::Ref{Cint})::Ptr{Cint}
	return @ccall _HOFEM_LIB_PATH.jl_get_local_points_with_fake_IDS_of_this_face(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, face::Ref{Cint})::Ptr{Cint}
end

function get_all_local_nodes_IDS_of_this_edge(mesh::MeshObject, elemID::Ref{Cint}, edge::Ref{Cint})::Ptr{Cint}
	return @ccall _HOFEM_LIB_PATH.jl_get_all_local_nodes_IDS_of_this_edge(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, edge::Ref{Cint})::Ptr{Cint}
end

function get_all_local_nodes_IDS_of_this_face(mesh::MeshObject, elemID::Ref{Cint}, face::Ref{Cint})::Ptr{Cint}
	return @ccall _HOFEM_LIB_PATH.jl_get_all_local_nodes_IDS_of_this_face(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, face::Ref{Cint})::Ptr{Cint}
end

function get_lagrange_local_nodes_of_this_face(mesh::MeshObject, elemID::Ref{Cint}, face::Ref{Cint})::Ptr{Cint}
	return @ccall _HOFEM_LIB_PATH.jl_get_lagrange_local_nodes_of_this_face(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, face::Ref{Cint})::Ptr{Cint}
end

function get_number_of_faces_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_number_of_faces_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_number_of_edges_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_number_of_edges_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_number_of_vertices_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_number_of_vertices_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_dof_per_volume_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_dof_per_volume_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_dof_per_face_of_this_element(mesh::MeshObject, elemID::Ref{Cint}, face::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_dof_per_face_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint}, face::Ref{Cint})::Cint
end

function get_dof_per_edge_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_dof_per_edge_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function get_dof_per_vertex_of_this_element(mesh::MeshObject, elemID::Ref{Cint})::Cint
	return @ccall _HOFEM_LIB_PATH.jl_get_dof_per_vertex_of_this_element(mesh.handle::Ptr{Cvoid}, elemID::Ref{Cint})::Cint
end

function create_element_type_objects_in_mesh(mesh::MeshObject)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_create_element_type_objects_in_mesh(mesh.handle::Ptr{Cvoid})::Cvoid
end

function deallocate_mesh_array_memory(mesh::MeshObject)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_deallocate_mesh_array_memory(mesh.handle::Ptr{Cvoid})::Cvoid
end


