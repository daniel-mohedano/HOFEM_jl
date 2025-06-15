"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 15 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for program_parallel_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""
_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")




#@C Anything inside this section will be preserved by the builder
function initiate_parallel_environment()
  @ccall _HOFEM_LIB_PATH.program_parallel_module_mp_initiate_parallel_environment_()::Cvoid
end
#/@C
