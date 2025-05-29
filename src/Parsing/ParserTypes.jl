abstract type AbstractParser end
abstract type AbstractModule end

abstract type AbstractType end

@enum Visibility Public Private

struct VariableAttrs
  is_parameter::Bool
  is_allocatable::Bool
  is_pointer::Bool
  is_target::Bool
  dimensions::Union{Vector{AbstractString},Nothing}
  intent::Union{AbstractString,Nothing}  # in, out, inout
end
VariableAttrs() = VariableAttrs(false, false, false, false, nothing, nothing)

function Base.show(io::IO, ::MIME"text/plain", attr::VariableAttrs)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  is_parameter: ", attr.is_parameter)
  println(io, tab, "  is_allocatable: ", attr.is_allocatable)
  println(io, tab, "  is_pointer: ", attr.is_pointer)
  println(io, tab, "  is_target: ", attr.is_target)
  println(io, tab, "  dimensions: ", isnothing(attr.dimensions) || isempty(attr.dimensions) ? "-" : attr.dimensions)
  println(io, tab, "  intent: ", attr.intent === nothing ? "-" : attr.intent)
end

struct IntrinsicType <: AbstractType
  name::AbstractString
  kind::Union{AbstractString,Nothing}
  len::Union{AbstractString,Nothing}
end
IntrinsicType(name::AbstractString) = IntrinsicType(name, nothing, nothing)

function fortran_type(type::IntrinsicType)
  if lowercase(type.name) == "integer"
    return "C_INT"
  elseif lowercase(type.name) == "real"
    if isnothing(type.kind) || type.kind == "DBL"
      return "C_DOUBLE"
    else
      return "C_FLOAT"
    end
  elseif lowercase(type.name) == "logical"
    return "C_BOOL"
  elseif lowercase(type.name) == "complex"
    if isnothing(type.kind) || type.kind == "DBL"
      return "C_DOUBLE_COMPLEX"
    else
      return "C_FLOAT_COMPLEX"
    end
  else
    return ""
  end
end

function julia_type(type::IntrinsicType)
  if lowercase(type.name) == "integer"
    return "Cint"
  elseif lowercase(type.name) == "real"
    if isnothing(type.kind) || type.kind == "DBL"
      return "Cdouble"
    else
      return "Cfloat"
    end
  elseif lowercase(type.name) == "logical"
    return "Cuchar"
  elseif lowercase(type.name) == "complex"
    if isnothing(type.kind) || type.kind == "DBL"
      return "ComplexF64"
    else
      return "ComplexF32"
    end
  else
    return ""
  end
end

function Base.show(io::IO, ::MIME"text/plain", type::IntrinsicType)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  name: ", type.name)
  println(io, tab, "  kind: ", type.kind === nothing ? "-" : type.kind)
  println(io, tab, "  len: ", type.len === nothing ? "-" : type.len)
end

# Fortran variable
struct Variable{T<:AbstractType}
  name::AbstractString
  type::T
  attributes::VariableAttrs
end
Variable(name::AbstractString, type::T) where {T<:AbstractType} = Variable{T}(name, type, VariableAttrs())

function Base.show(io::IO, ::MIME"text/plain", var::Variable)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  name: ", var.name)
  println(io, tab, "  type: ")
  show(IOContext(io, :indent => indent + 2), MIME"text/plain"(), var.type)
  println(io, tab, "  attributes: ")
  show(IOContext(io, :indent => indent + 2), MIME"text/plain"(), var.attributes)
end

struct DerivedTypeAttrs
  is_abstract::Bool
  is_public::Bool
  extends::Union{AbstractString,Nothing}  # Name of the parent type if extends
end
DerivedTypeAttrs() = DerivedTypeAttrs(false, true, nothing)

function Base.show(io::IO, ::MIME"text/plain", attrs::DerivedTypeAttrs)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  is_abstract: ", attrs.is_abstract)
  println(io, tab, "  is_public: ", attrs.is_public)
  println(io, tab, "  extends: ", attrs.extends === nothing ? "-" : attrs.extends)
end

struct DerivedType <: AbstractType
  name::AbstractString
  members::Vector{Variable}
  attributes::DerivedTypeAttrs
end
DerivedType(name::AbstractString, members::Vector{Variable}) = DerivedType(name, members, DerivedTypeAttrs())
DerivedType(name::AbstractString) = DerivedType(name, Variable[], DerivedTypeAttrs())

function Base.show(io::IO, ::MIME"text/plain", type::DerivedType)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  name: ", type.name)
  println(io, tab, "  attributes: ")
  show(IOContext(io, :indent => indent + 2), MIME"text/plain"(), type.attributes)
  println(io, tab, "  members: ")
  for (i, member) in enumerate(type.members)
    println(io, tab, "    member ", i, ":")
    show(IOContext(io, :indent => indent + 4), MIME"text/plain"(), member)
  end
end

struct Procedure
  name::AbstractString
  args::Vector{Variable}
  ret::Union{Variable,Nothing}  # Nothing for subroutines
  is_pure::Bool
  is_elemental::Bool
  visibility::Visibility
end
Procedure(name::AbstractString, args::Vector{Variable}, ret::Union{<:AbstractType,Nothing}=nothing) = Procedure(name, args, ret, false, false, Public)

function Base.show(io::IO, ::MIME"text/plain", proc::Procedure)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  name: ", proc.name)
  println(io, tab, "  is_pure: ", proc.is_pure)
  println(io, tab, "  is_elemental: ", proc.is_elemental)
  println(io, tab, "  visibility: ", proc.visibility)
  println(io, tab, "  args: ")
  for (i, arg) in enumerate(proc.args)
    println(io, tab, "    arg ", i, ":")
    show(IOContext(io, :indent => indent + 4), MIME"text/plain"(), arg)
  end
  println(io, tab, "  return: ", proc.ret === nothing ? "None (subroutine)" : "")
  if proc.ret !== nothing
    show(IOContext(io, :indent => indent + 2), MIME"text/plain"(), proc.ret)
  end
end

struct ModuleVariable
  var::Variable
  visibility::Visibility
end

function Base.show(io::IO, ::MIME"text/plain", modvar::ModuleVariable)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  visibility: ", modvar.visibility)
  println(io, tab, "  variable: ")
  show(IOContext(io, :indent => indent + 2), MIME"text/plain"(), modvar.var)
end

struct Module <: AbstractModule
  name::AbstractString
  types::Vector{DerivedType}
  variables::Vector{ModuleVariable}
  procedures::Vector{Procedure}
end
Module(name::AbstractString) = Module(name, DerivedType[], ModuleVariable[], Procedure[])

function Base.show(io::IO, ::MIME"text/plain", mod::Module)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  name: ", mod.name)
  println(io, tab, "  types: ")
  for (i, type) in enumerate(mod.types)
    println(io, tab, "    type ", i, ":")
    show(IOContext(io, :indent => indent + 4), MIME"text/plain"(), type)
  end
  println(io, tab, "  variables: ")
  for (i, var) in enumerate(mod.variables)
    println(io, tab, "    variable ", i, ":")
    show(IOContext(io, :indent => indent + 4), MIME"text/plain"(), var)
  end
  println(io, tab, "  procedures: ")
  for (i, proc) in enumerate(mod.procedures)
    println(io, tab, "    procedure ", i, ":")
    show(IOContext(io, :indent => indent + 4), MIME"text/plain"(), proc)
  end
end
