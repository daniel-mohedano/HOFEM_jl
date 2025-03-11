"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 7 October 2024
!
! DESCRIPTION:
!> Automatically generated Julia interface module for mesh_object_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")

function MeshObject_set_geo_order!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_geo_order(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_geo_order(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_geo_order(data_c_ptr::Ptr{Cvoid})::Cint
end

function MeshObject_set_var_order!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_var_order(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_var_order(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_var_order(data_c_ptr::Ptr{Cvoid})::Cint
end

function MeshObject_set_num_dofs!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_num_dofs(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_num_dofs(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_num_dofs(data_c_ptr::Ptr{Cvoid})::Cint
end

function MeshObject_set_number_of_points!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_number_of_points(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_number_of_points(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_number_of_points(data_c_ptr::Ptr{Cvoid})::Cint
end

function MeshObject_set_number_of_tetrahedras!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_number_of_tetrahedras(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_number_of_tetrahedras(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_number_of_tetrahedras(data_c_ptr::Ptr{Cvoid})::Cint
end

function MeshObject_set_number_of_prisms!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_number_of_prisms(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_number_of_prisms(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_number_of_prisms(data_c_ptr::Ptr{Cvoid})::Cint
end

function MeshObject_set_number_of_hexahedras!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_number_of_hexahedras(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_number_of_hexahedras(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_number_of_hexahedras(data_c_ptr::Ptr{Cvoid})::Cint
end

function MeshObject_set_lower_dof_id!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_lower_dof_id(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_lower_dof_id(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_lower_dof_id(data_c_ptr::Ptr{Cvoid})::Cint
end

function MeshObject_set_upper_dof_id!(data_c_ptr::Ptr{Cvoid}, val::Cint)
  @ccall _HOFEM_LIB_PATH.meshobject_set_upper_dof_id(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function MeshObject_get_upper_dof_id(data_c_ptr::Ptr{Cvoid})::Cint
  return @ccall _HOFEM_LIB_PATH.meshobject_get_upper_dof_id(data_c_ptr::Ptr{Cvoid})::Cint
end

function get_local_mesh()::Ptr{Cvoid}
  return @ccall _HOFEM_LIB_PATH.get_local_mesh()::Ptr{Cvoid}
end

function get_global_mesh()::Ptr{Cvoid}
  return @ccall _HOFEM_LIB_PATH.get_global_mesh()::Ptr{Cvoid}
end

function get_IIEE_mesh()::Ptr{Cvoid}
  return @ccall _HOFEM_LIB_PATH.get_iiee_mesh()::Ptr{Cvoid}
end

function print_MeshObject(data_c_ptr::Ptr{Cvoid})
  @ccall _HOFEM_LIB_PATH.print_meshobject(data_c_ptr::Ptr{Cvoid})::Cvoid
end



#@C Anything inside this section will be preserved by the builder

#/@C
