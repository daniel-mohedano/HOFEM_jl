"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for mesh_interface_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
#@C Anything inside this section will be preserved by the builder
function mesh_interface_constructor!(mesh::Ptr{Cvoid}, meshfilename::String)
  @ccall _HOFEM_LIB_PATH.jl_mesh_interface_constructor(meshfilename::Ptr{Cchar}, mesh::Ptr{Cvoid})::Cvoid
end
#/@C



