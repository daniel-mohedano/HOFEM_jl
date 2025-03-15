abstract type Language end
struct Julia <: Language end
struct Fortran <: Language end

# Abstract Fortran construct (types and routines)
abstract type AbstractFConstruct end
abstract type AbstractFType <: AbstractFConstruct end

# Visibility of module members
@enum Visibility Public Private

# Variable attributes
struct FVarAttributes
  is_parameter::Bool
  is_allocatable::Bool
  is_pointer::Bool
  dimensions::Union{Vector{Union{Int,AbstractString}},Nothing}
  intent::Union{AbstractString,Nothing}  # in, out, inout
end
FVarAttributes() = FVarAttributes(false, false, false, nothing, nothing)

# Intrinsic Fortran type with kind parameter
struct FIntrinsic <: AbstractFType
  name::AbstractString
  kind::Union{Int,AbstractString,Nothing}
end
FIntrinsic(name::AbstractString) = FIntrinsic(name, nothing)

# Fortran variable
struct FVar{T<:AbstractFType}
  name::AbstractString
  type::T
  attributes::FVarAttributes
end
FVar(name::AbstractString, type::T) where {T<:AbstractFType} = FVar{T}(name, type, FVarAttributes())

# Derived type attributes
struct FDerivedAttributes
  is_abstract::Bool
  is_public::Bool
  extends::Union{AbstractString,Nothing}  # Name of the parent type if extends
end
FDerivedAttributes() = FDerivedAttributes(false, true, nothing)

# Derived Fortran type with members
struct FDerived <: AbstractFType
  name::AbstractString
  members::Vector{FVar}
  attributes::FDerivedAttributes
end
FDerived(name::AbstractString, members::Vector{FVar}) = FDerived(name, members, FDerivedAttributes())

# Generic Fortran procedure (function or subroutine)
struct FProcedure <: AbstractFConstruct
  name::AbstractString
  args::Vector{FVar}
  ret::Union{<:AbstractFType,Nothing}  # Nothing for subroutines
  is_pure::Bool
  is_elemental::Bool
  visibility::Visibility
end
FProcedure(name::AbstractString, args::Vector{FVar}, ret::Union{<:AbstractFType,Nothing}=nothing) = FProcedure(name, args, ret, false, false, Public)

# Module variable or constant
struct FModuleVar <: AbstractFConstruct
  var::FVar
  visibility::Visibility
end

# Fortran module
struct FModule <: AbstractFConstruct
  name::AbstractString
  types::Vector{FDerived}
  variables::Vector{FModuleVar}
  procedures::Vector{FProcedure}
end
FModule(name::AbstractString) = FModule(name, FDerived[], FModuleVar[], FProcedure[])
