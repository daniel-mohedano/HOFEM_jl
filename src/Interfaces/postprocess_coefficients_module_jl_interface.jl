"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for postprocess_coefficients_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
#@C Anything inside this section will be preserved by the builder
function read_coefficients_from_postprocess_file(frequency::Integer, rhs::Integer, n_rhs::Integer, n_coeffs::Integer, project_name::String)::Array{ComplexF64}
  coefficients = zeros(ComplexF64, n_coeffs)
  header = zeros(Cint, 3)
  postprocess_file = joinpath(strip(project_name) * "_POST", strip(splitpath(project_name)[end]) * ".data.post")
  record_pos = 0
  open(postprocess_file) do io
    # Get length of one frequency and rhs
    skip(io, 4) # Skip random record width inserted by Fortran
    read!(io, header)
    # Get record header
    record_header = 8 + 3 * 4 + 8
    seekstart(io)
    skip(io, 4) # Skip random record width inserted by Fortran
    # Go over all lower frequencies
    for ii = 1:(frequency-1)
      for jj = 1:n_rhs
        # Move pointer over header
        skip(io, record_header)
        for kk = 1:ceil(Float64(1.0) * header[3] / 10000000)
          dof_lower = (kk - 1) * 10000000 + 1
          dof_upper = kk * 10000000
          if dof_upper > header[3]
            dof_upper = header[3]
          end
          record_pos = (dof_upper - dof_lower + 1) * 16
          skip(io, record_pos)
        end
      end
    end
    # Move to desired rhs
    for jj = 1:rhs-1
      skip(io, record_header)
      for kk = 1:ceil(Float64(1.0) * header[3] / 10000000)
        dof_lower = (kk - 1) * 10000000 + 1
        dof_upper = kk * 10000000
        if dof_upper > header[3]
          dof_upper = header[3]
        end
        record_pos = (dof_upper - dof_lower + 1) * 16
        skip(io, record_pos)
      end
    end

    # Read header of the file
    read!(io, header)
    skip(io, 8)
    # Read coefficients
    read!(io, coefficients)
  end
  return coefficients
end
#/@C

function write_thermal_coefficients_into_postprocess_file(mesh::MeshObject, time_step::Ref{Cdouble})::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_write_thermal_coefficients_into_postprocess_file(mesh.handle::Ptr{Cvoid}, time_step::Ref{Cdouble})::Cvoid
end

function write_coefficients_into_postprocess_file(mesh::MeshObject, frequency::Ref{Cint}, rhs_lower::Ref{Cint}, rhs_upper::Ref{Cint})::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_write_coefficients_into_postprocess_file(mesh.handle::Ptr{Cvoid}, frequency::Ref{Cint}, rhs_lower::Ref{Cint}, rhs_upper::Ref{Cint})::Cvoid
end

function copy_adaptivity_coefficients_into_postprocess_file(frequency::Ref{Cint})::Cvoid
	return @ccall _HOFEM_LIB_PATH.postprocess_coefficients_module_mp_copy_adaptivity_coefficients_into_postprocess_file_(frequency::Ref{Cint})::Cvoid
end

function postprocess_get_gid_gauss_points(mesh::MeshObject, gauss_points::Ptr{Cdouble})::Cvoid
	return @ccall _HOFEM_LIB_PATH.jl_postprocess_get_gid_gauss_points(mesh.handle::Ptr{Cvoid}, gauss_points::Ptr{Cdouble})::Cvoid
end


