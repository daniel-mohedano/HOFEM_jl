# HOFEM_jl

A Julia library for parsing Fortran modules and automatically generating Julia interfaces for calling Fortran code.

## Features

- **Fortran Module Parsing**: Extracts subroutines, functions, derived types, and module variables from Fortran source files
- **Automatic Interface Generation**: Creates Julia wrappers with proper C interoperability for Fortran-Julia integration
- **Type-Safe Wrappers**: Generates C-compatible interfaces with proper type conversions

## Quick Start

```julia
using HOFEM_jl

# Parse a Fortran module
module_data = parse_fortran_module("path/to/module.f90")

# Generate Julia interfaces
generate_interfaces(module_data, "output/directory")
```

## Generated Interface Example

**Fortran Code:**

```fortran
type :: point
    real :: x, y
end type point

function distance(p1, p2) result(d)
    type(point), intent(in) :: p1, p2
    real :: d
end function distance
```

**Generated Julia Interface:**

```julia
struct point
    handle::Ptr{Cvoid}
end

function distance(p1::point, p2::point)::Cdouble
    return @ccall _HOFEM_LIB_PATH.call_distance(p1.handle, p2.handle)::Cdouble
end

function get_x(obj::point)::Cdouble
    return @ccall _HOFEM_LIB_PATH.point_get_x(obj.handle::Ptr{Cvoid})::Cdouble
end
```
