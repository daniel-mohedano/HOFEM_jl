abstract type AbstractParser end
abstract type AbstractModule end

abstract type AbstractType end

@enum Visibility Public Private

struct VariableAttrs
  is_parameter::Bool
  is_allocatable::Bool
  is_pointer::Bool
  is_target::Bool
  is_value::Bool
  dimensions::Union{Vector{AbstractString},Nothing}
  intent::Union{AbstractString,Nothing}  # in, out, inout
end
VariableAttrs() = VariableAttrs(false, false, false, false, false, nothing, nothing)

function Base.show(io::IO, ::MIME"text/plain", attr::VariableAttrs)
  indent = get(io, :indent, 0)
  tab = ' '^indent
  println(io, tab, "  is_parameter: ", attr.is_parameter)
  println(io, tab, "  is_allocatable: ", attr.is_allocatable)
  println(io, tab, "  is_pointer: ", attr.is_pointer)
  println(io, tab, "  is_target: ", attr.is_target)
  println(io, tab, "  is_value: ", attr.is_value)
  println(io, tab, "  dimensions: ", isnothing(attr.dimensions) || isempty(attr.dimensions) ? "-" : attr.dimensions)
  println(io, tab, "  intent: ", attr.intent === nothing ? "-" : attr.intent)
end

struct IntrinsicType <: AbstractType
  name::AbstractString
  kind::Union{AbstractString,Nothing}
  len::Union{AbstractString,Nothing}
end
IntrinsicType(name::AbstractString) = IntrinsicType(name, nothing, nothing)

# Map for Int and Real types depending on kind
const F_INT = Dict("1" => "C_INT8_T", "2" => "C_INT16_T", "4" => "C_INT32_T", "8" => "C_INT64_T")
const F_REAL = Dict("4" => "C_FLOAT", "8" => "C_DOUBLE", "DBL" => "C_DOUBLE")
const F_COMPLEX = Dict("4" => "C_FLOAT_COMPLEX", "8" => "C_DOUBLE_COMPLEX", "DBL" => "C_DOUBLE_COMPLEX")

function _intrinsic_map(t::IntrinsicType)::Tuple{Bool,Union{String,Nothing}}
  lname = lowercase(t.name)
  kind = isnothing(t.kind) ? "" : t.kind
  if lname == "integer"
    iso = get(F_INT, kind, "C_INT")
    return true, "INTEGER($(iso))"
  elseif lname == "real"
    iso = get(F_REAL, kind, "C_DOUBLE")
    return true, "REAL($(iso))"
  elseif lname == "complex"
    iso = get(F_COMPLEX, kind, "C_DOUBLE_COMPLEX")
    return true, "COMPLEX($(iso))"
  elseif lname == "logical"
    return true, "LOGICAL(C_BOOL)"
  elseif lname == "character"
    return true, "CHARACTER(KIND=C_CHAR), DIMENSION(*)"
  else
    return false, nothing
  end
end

function fortran_type(t::IntrinsicType)::String
  m = _intrinsic_map(t)[2]
  return isnothing(m) ? "" : m
end

const JL_INT = Dict("1" => Int8, "2" => Int16, "4" => Int32, "8" => Int64)
const JL_REAL = Dict("4" => Float32, "8" => Float64)
const JL_COMPLEX = Dict("4" => ComplexF32, "8" => ComplexF64)

function julia_type(t::IntrinsicType)::String
  lname = lowercase(t.name)
  kind = isnothing(t.kind) ? "" : t.kind
  if lname == "integer"
    return string(get(JL_INT, kind, Int32))
  elseif lname == "real"
    return string(get(JL_REAL, kind, Float64))
  elseif lname == "complex"
    return string(get(JL_COMPLEX, kind, ComplexF64))
  elseif lname == "logical"
    return "Cuchar"
  elseif lname == "character"
    return "Ptr{Cchar}"
  else
    return "Ptr{Cvoid}"
  end
end

function is_interoperable(t::IntrinsicType)::Bool
  # todo: modify this, depending on the kind
  #return isnothing(t.kind) || occursin("c_", lowercase(t.kind))
  return _intrinsic_map(t)[1]
end

function isstring(type::IntrinsicType)::Bool
  # todo: implemente as overload of isa
  return (lowercase(type.name) == "char" || lowercase(type.name) == "character") && !isnothing(type.len)
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

function is_interoperable(v::Variable)::Bool
  at = v.attributes

  # pointers, allocatable and targets need descriptors
  if at.is_pointer || at.is_allocatable || at.is_target
    return false
  end

  # assumed-shape are non-interoperable
  if !isnothing(at.dimensions) && any(d -> d == ":", at.dimensions)
    return false
  end

  return is_interoperable(v.type)
end

function julia_type(v::Variable)::String
  if !is_interoperable(v)
    return "Ptr{Cvoid}"
  end

  return julia_type(v.type)
end

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

function is_interoperable(t::DerivedType)::Bool
  return false
end

function julia_type(t::DerivedType)::String
  return "Ptr{Cvoid}"
end

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
