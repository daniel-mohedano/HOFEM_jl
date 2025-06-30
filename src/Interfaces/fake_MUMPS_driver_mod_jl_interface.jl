"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for fake_MUMPS_driver_mod_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
#@C Anything inside this section will be preserved by the builder

#/@C

function fake_dMUMPS_driver_centralized(FEM_matrix_real::SparseMatrix_type_real, DenseRHS_real::SparseMatrix_type_real)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_fake_dMUMPS_driver_centralized(FEM_matrix_real.handle::Ptr{Cvoid}, DenseRHS_real.handle::Ptr{Cvoid})::Cvoid
end

function fake_zMUMPS_driver_centralized(FEM_matrix_complex::SparseMatrix_type_complex, DenseRHS_complex::SparseMatrix_type_complex)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_fake_zMUMPS_driver_centralized(FEM_matrix_complex.handle::Ptr{Cvoid}, DenseRHS_complex.handle::Ptr{Cvoid})::Cvoid
end

function fake_dMUMPS_driver_distributed(FEM_matrix_real::SparseMatrix_type_real, DenseRHS_real::SparseMatrix_type_real)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_fake_dMUMPS_driver_distributed(FEM_matrix_real.handle::Ptr{Cvoid}, DenseRHS_real.handle::Ptr{Cvoid})::Cvoid
end

function fake_zMUMPS_driver_distributed(FEM_matrix_complex::SparseMatrix_type_complex, DenseRHS_complex::SparseMatrix_type_complex)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_fake_zMUMPS_driver_distributed(FEM_matrix_complex.handle::Ptr{Cvoid}, DenseRHS_complex.handle::Ptr{Cvoid})::Cvoid
end


