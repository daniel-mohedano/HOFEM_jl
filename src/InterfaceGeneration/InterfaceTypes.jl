
abstract type Language end
using Base: isiterable
struct Julia <: Language end
struct Fortran <: Language end

# Abstract Fortran construct (types and routines)
abstract type AbstractFConstruct end
# Abstract Fortran type
abstract type AbstractFType <: AbstractFConstruct end

# Intrinsic Fortran type
struct FIntrinsic <: AbstractFType
  name::String
end

# Fortran variable
struct FVar{T<:AbstractFType}
  name::String
  type::T
end

# Derived Fortran type with members
struct FDerived <: AbstractFType
  name::String
  members::Vector{FVar}
end

HOFEM_jl.isiterable(::FDerived) = false

# Generic Fortran function (a subroutine would have no return type)
struct FFunction <: AbstractFConstruct
  name::String
  args::Vector{FVar}
  ret::Union{<:AbstractFType,Nothing}
end

struct FModule <: AbstractFConstruct
  name::String
  content::Vector{<:AbstractFConstruct}
end
