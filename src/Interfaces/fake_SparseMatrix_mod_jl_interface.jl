"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for fake_SparseMatrix_mod_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
#@C Anything inside this section will be preserved by the builder

#/@C

struct SparseMatrix_type
	handle::Ptr{Cvoid}
end

function SparseMatrix_type()
	return SparseMatrix_type(@ccall _HOFEM_LIB_PATH.new_sparsematrix_type()::Ptr{Cvoid})
end

function Base.finalize(obj::SparseMatrix_type)
	@ccall _HOFEM_LIB_PATH.free_sparsematrix_type(obj.handle::Ptr{Cvoid})::Cvoid
end

function set_n!(obj::SparseMatrix_type, val::Cint)
	@ccall _HOFEM_LIB_PATH.sparsematrix_type_set_n(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_n(obj::SparseMatrix_type)::Cint
	return @ccall _HOFEM_LIB_PATH.sparsematrix_type_get_n(obj.handle::Ptr{Cvoid})::Cint
end

function set_nnz!(obj::SparseMatrix_type, val::Cint)
	@ccall _HOFEM_LIB_PATH.sparsematrix_type_set_nnz(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_nnz(obj::SparseMatrix_type)::Cint
	return @ccall _HOFEM_LIB_PATH.sparsematrix_type_get_nnz(obj.handle::Ptr{Cvoid})::Cint
end

function set_mat_type_format!(obj::SparseMatrix_type, val::Cint)
	@ccall _HOFEM_LIB_PATH.sparsematrix_type_set_mat_type_format(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_mat_type_format(obj::SparseMatrix_type)::Cint
	return @ccall _HOFEM_LIB_PATH.sparsematrix_type_get_mat_type_format(obj.handle::Ptr{Cvoid})::Cint
end

function set_flag_sym!(obj::SparseMatrix_type, val::Cint)
	@ccall _HOFEM_LIB_PATH.sparsematrix_type_set_flag_sym(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_flag_sym(obj::SparseMatrix_type)::Cint
	return @ccall _HOFEM_LIB_PATH.sparsematrix_type_get_flag_sym(obj.handle::Ptr{Cvoid})::Cint
end

function set_flag_complex!(obj::SparseMatrix_type, val::Cint)
	@ccall _HOFEM_LIB_PATH.sparsematrix_type_set_flag_complex(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_flag_complex(obj::SparseMatrix_type)::Cint
	return @ccall _HOFEM_LIB_PATH.sparsematrix_type_get_flag_complex(obj.handle::Ptr{Cvoid})::Cint
end

struct SparseMatrix_type_real
	handle::Ptr{Cvoid}
end

function SparseMatrix_type_real()
	return SparseMatrix_type_real(@ccall _HOFEM_LIB_PATH.new_sparsematrix_type_real()::Ptr{Cvoid})
end

function Base.finalize(obj::SparseMatrix_type_real)
	@ccall _HOFEM_LIB_PATH.free_sparsematrix_type_real(obj.handle::Ptr{Cvoid})::Cvoid
end

function print_SparseMatrix_type_real(obj::SparseMatrix_type_real)
	@ccall _HOFEM_LIB_PATH.print_sparsematrix_type_real(obj.handle::Ptr{Cvoid})::Cvoid
end

struct SparseMatrix_type_complex
	handle::Ptr{Cvoid}
end

function SparseMatrix_type_complex()
	return SparseMatrix_type_complex(@ccall _HOFEM_LIB_PATH.new_sparsematrix_type_complex()::Ptr{Cvoid})
end

function Base.finalize(obj::SparseMatrix_type_complex)
	@ccall _HOFEM_LIB_PATH.free_sparsematrix_type_complex(obj.handle::Ptr{Cvoid})::Cvoid
end

function print_SparseMatrix_type_complex(obj::SparseMatrix_type_complex)
	@ccall _HOFEM_LIB_PATH.print_sparsematrix_type_complex(obj.handle::Ptr{Cvoid})::Cvoid
end

function print_SparseMatrix_type(this::SparseMatrix_type)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_print_SparseMatrix_type(this.handle::Ptr{Cvoid})::Cvoid
end

function deallocate_SparseMatrix_type_components(this::SparseMatrix_type)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_deallocate_SparseMatrix_type_components(this.handle::Ptr{Cvoid})::Cvoid
end

function set_default_SparseMatrix_type(FEM_matrix::SparseMatrix_type)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_set_default_SparseMatrix_type(FEM_matrix.handle::Ptr{Cvoid})::Cvoid
end

function set_default_DenseRHS_type(DenseRHS::SparseMatrix_type)::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_set_default_DenseRHS_type(DenseRHS.handle::Ptr{Cvoid})::Cvoid
end

function set_SparseMatrix_type(FEM_matrix::SparseMatrix_type, n::Ref{Cint}, mat_type_format::Ref{Cint}, flag_sym::Ref{Cint}, IRN_size::Cint, IRN::Ptr{Cint}, JCN_size::Cint, JCN::Ptr{Cint})::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_set_SparseMatrix_type(FEM_matrix.handle::Ptr{Cvoid}, n::Ref{Cint}, mat_type_format::Ref{Cint}, flag_sym::Ref{Cint}, IRN_size::Cint, IRN::Ptr{Cint}, JCN_size::Cint, JCN::Ptr{Cint})::Cvoid
end

function set_DenseRHS_type(DenseRHS::SparseMatrix_type, n::Ref{Cint}, mat_type_format::Ref{Cint}, flag_sym::Ref{Cint})::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_set_DenseRHS_type(DenseRHS.handle::Ptr{Cvoid}, n::Ref{Cint}, mat_type_format::Ref{Cint}, flag_sym::Ref{Cint})::Cvoid
end


