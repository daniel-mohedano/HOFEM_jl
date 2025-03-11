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
    dimensions::Union{Vector{Union{Int,Symbol}}, Nothing}
    intent::Union{Symbol,Nothing}  # in, out, inout

    FVarAttributes() = new(false, false, false, nothing, nothing)
end

# Intrinsic Fortran type with kind parameter
struct FIntrinsic <: AbstractFType
    name::String
    kind::Union{Int,Symbol,Nothing}
end

# Fortran variable
struct FVar{T<:AbstractFType}
    name::String
    type::T
    attributes::FVarAttributes

    FVar(name::String, type::T) where {T<:AbstractFType} = new{T}(name, type, FVarAttributes())
end

# Derived type attributes
struct FDerivedAttributes
    is_abstract::Bool
    extends::Union{String,Nothing}  # Name of the parent type if extends
    is_public::Bool

    FDerivedAttributes() = new(false, nothing, true)
end

# Derived Fortran type with members
struct FDerived <: AbstractFType
    name::String
    members::Vector{FVar}
    attributes::FDerivedAttributes

    FDerived(name::String, members::Vector{FVar}) = new(name, members, FDerivedAttributes())
end

# Generic Fortran procedure (function or subroutine)
struct FProcedure <: AbstractFConstruct
    name::String
    args::Vector{FVar}
    ret::Union{<:AbstractFType,Nothing}  # Nothing for subroutines
    is_pure::Bool
    is_elemental::Bool
    visibility::Visibility

    FProcedure(name::String, args::Vector{FVar}, ret::Union{<:AbstractFType,Nothing}=nothing) = 
        new(name, args, ret, false, false, Public)
end

# Module variable or constant
struct FModuleVar <: AbstractFConstruct
    var::FVar
    visibility::Visibility
end

# Fortran module
struct FModule <: AbstractFConstruct
    name::String
    types::Vector{FDerived}
    variables::Vector{FModuleVar}
    procedures::Vector{FProcedure}

    FModule(name::String) = new(name, FDerived[], FModuleVar[], FProcedure[])
end