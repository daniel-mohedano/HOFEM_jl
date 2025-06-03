"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 3 June 2025
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
