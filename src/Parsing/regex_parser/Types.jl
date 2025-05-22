#macro exportinstances(enum)
#  # from: https://discourse.julialang.org/t/how-to-write-a-macro-to-export-all-instances-of-an-enum-type/73137
#  eval = GlobalRef(Core, :eval)
#  return :($eval($__module__, Expr(:export, map(Symbol, instances($enum))...)))
#end

abstract type AbstractType end

@enum Visibility Public Private
#@exportinstances Visibility

struct VariableAttrs
  is_parameter::Bool
  is_allocatable::Bool
  is_pointer::Bool
  dimensions::Union{Vector{AbstractString},Nothing}
  intent::Union{AbstractString,Nothing}  # in, out, inout
end
VariableAttrs() = VariableAttrs(false, false, false, nothing, nothing)

struct IntrinsicType <: AbstractType
  name::AbstractString
  kind::Union{AbstractString,Nothing}
  len::Union{AbstractString,Nothing}
end
IntrinsicType(name::AbstractString) = IntrinsicType(name, nothing, nothing)

# Fortran variable
struct Variable{T<:AbstractType}
  name::AbstractString
  type::T
  attributes::VariableAttrs
end
Variable(name::AbstractString, type::T) where {T<:AbstractType} = Variable{T}(name, type, VariableAttrs())

struct DerivedTypeAttrs
  is_abstract::Bool
  is_public::Bool
  extends::Union{AbstractString,Nothing}  # Name of the parent type if extends
end
DerivedTypeAttrs() = DerivedTypeAttrs(false, true, nothing)

struct DerivedType <: AbstractType
  name::AbstractString
  members::Vector{Variable}
  attributes::DerivedTypeAttrs
end
DerivedType(name::AbstractString, members::Vector{Variable}) = DerivedType(name, members, DerivedTypeAttrs())
DerivedType(name::AbstractString) = DerivedType(name, Variable[], DerivedTypeAttrs())

struct Procedure
  name::AbstractString
  args::Vector{Variable}
  ret::Union{Variable,Nothing}  # Nothing for subroutines
  is_pure::Bool
  is_elemental::Bool
  visibility::Visibility
end
Procedure(name::AbstractString, args::Vector{Variable}, ret::Union{<:AbstractType,Nothing}=nothing) = Procedure(name, args, ret, false, false, Public)

struct ModuleVariable
  var::Variable
  visibility::Visibility
end

struct Module <: AbstractModule
  name::AbstractString
  types::Vector{DerivedType}
  variables::Vector{ModuleVariable}
  procedures::Vector{Procedure}
end
Module(name::AbstractString) = Module(name, DerivedType[], ModuleVariable[], Procedure[])
