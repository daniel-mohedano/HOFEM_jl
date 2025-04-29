# Fortran AST structures
# This file contains the Julia structures that represent the essential parts of the Fortran AST

using JSON3, StructTypes

# Dictionary mapping ASDL type names to Julia types
const ASDL_TYPES = Dict{String,Type}()

struct Location
  first::Int
  last::Int
  first_filename::String
  first_line::Int
  first_column::Int
  last_filename::String
  last_line::Int
  last_column::Int
end
StructTypes.StructType(::Type{Location}) = StructTypes.Struct()
ASDL_TYPES["Location"] = Location

# Base struct for all AST nodes
struct ASTNode
  node::String
  fields::Dict{String,Any}
  loc::Location
end
StructTypes.StructType(::Type{ASTNode}) = StructTypes.Struct()
ASDL_TYPES["ASTNode"] = ASTNode

abstract type DeclAttributeNode end

@enum DimensionType begin
  DimensionExpr
  DimensionStar
  AssumedRank
end

struct Dimension
  end_star::DimensionType
end
StructTypes.StructType(::Type{Dimension}) = StructTypes.Struct()
ASDL_TYPES["Dimension"] = Dimension

struct AttrDimensionNode <: DeclAttributeNode
  dim::Vector{Dimension}
end
StructTypes.StructType(::Type{AttrDimensionNode}) = StructTypes.Struct()
ASDL_TYPES["AttrDimension"] = AttrDimensionNode

struct AttrExtendsNode <: DeclAttributeNode
  name::String
end
StructTypes.StructType(::Type{AttrExtendsNode}) = StructTypes.Struct()
ASDL_TYPES["AttrExtends"] = AttrExtendsNode

@enum AttrIntent begin
  In
  Out
  InOut
end

struct AttrIntentNode <: DeclAttributeNode
  intent::AttrIntent
end
StructTypes.StructType(::Type{AttrIntentNode}) = StructTypes.Struct()
ASDL_TYPES["AttrIntent"] = AttrIntentNode

@enum SimpleAttribute begin
  AttrAbstract
  AttrAllocatable
  AttrAsynchronous
  AttrContiguous
  AttrDeferred
  AttrElemental
  AttrEnumerator
  AttrExternal
  AttrImpure
  AttrIntrinsic
  AttrKind
  AttrLen
  AttrModule
  AttrNoPass
  AttrNonDeferred
  AttrNon_Intrinsic
  AttrOptional
  AttrParameter
  AttrPointer
  AttrPrivate
  AttrProtected
  AttrPublic
  AttrPure
  AttrRecursive
  AttrSave
  AttrSequence
  AttrTarget
  AttrValue
  AttrVolatile
end

struct SimpleAttributeNode <: DeclAttributeNode
  attr::SimpleAttribute
end
StructTypes.StructType(::Type{SimpleAttributeNode}) = StructTypes.Struct()
ASDL_TYPES["SimpleAttribute"] = SimpleAttributeNode

@enum DeclType begin
  TypeClass
  TypeCharacter
  TypeComplex
  TypeDoublePrecision
  TypeDoubleComplex
  TypeInteger
  TypeLogical
  TypeProcedure
  TypeReal
  TypeType
end

@enum KindItemType begin
  Star
  Colon
  Value
end

struct KindItemNode
  end_star::KindItemType
end
StructTypes.StructType(::Type{KindItemNode}) = StructTypes.Struct()
ASDL_TYPES["KindItem"] = KindItemNode

struct AttrTypeNode <: DeclAttributeNode
  type::DeclType
  kind::Vector{KindItemNode}
  attr::DeclAttributeNode
  name::String
end
StructTypes.StructType(::Type{AttrTypeNode}) = StructTypes.Struct()
ASDL_TYPES["AttrType"] = AttrTypeNode

abstract type ProgramUnitNode end

struct ArgNode
  arg::String
end
StructTypes.StructType(::Type{ArgNode}) = StructTypes.Struct()
ASDL_TYPES["Arg"] = ArgNode

struct SubroutineNode <: ProgramUnitNode
  name::String
  args::Vector{ArgNode}
  attributes::Vector{DeclAttributeNode}
end
StructTypes.StructType(::Type{SubroutineNode}) = StructTypes.Struct()
ASDL_TYPES["Subroutine"] = SubroutineNode

abstract type ExprNode end

struct Name <: ExprNode
  id::String
end
StructTypes.StructType(::Type{Name}) = StructTypes.Struct()
ASDL_TYPES["Name"] = Name

struct FunctionNode <: ProgramUnitNode
  name::String
  args::Vector{ArgNode}
  attributes::Vector{DeclAttributeNode}
  return_var::ExprNode
end
StructTypes.StructType(::Type{FunctionNode}) = StructTypes.Struct()
ASDL_TYPES["Function"] = FunctionNode

abstract type UnitDecl2Node end

struct VarSymNode
  name::String
  dim::Vector{Dimension}
end
StructTypes.StructType(::Type{VarSymNode}) = StructTypes.Struct()
ASDL_TYPES["VarSym"] = VarSymNode

struct DeclarationNode <: UnitDecl2Node
  vartype::DeclAttributeNode
  attributes::Vector{DeclAttributeNode}
  syms::Vector{VarSymNode}
end
StructTypes.StructType(::Type{DeclarationNode}) = StructTypes.Struct()
ASDL_TYPES["Declaration"] = DeclarationNode

struct DerivedTypeNode <: UnitDecl2Node
  name::String
  attrtype::Vector{DeclAttributeNode}
  items::Vector{UnitDecl2Node}
end
StructTypes.StructType(::Type{DerivedTypeNode}) = StructTypes.Struct()
ASDL_TYPES["DerivedType"] = DerivedTypeNode

struct ModuleNode
  name::String
  decl::Vector{UnitDecl2Node}
  contains::Vector{ProgramUnitNode}
end
StructTypes.StructType(::Type{ModuleNode}) = StructTypes.Struct()
ASDL_TYPES["Module"] = ModuleNode

# Update build_ast function to use JSON3
function build_ast(code::String)::Vector{Module}
  code_path, io = mktemp()
  write(io, code)
  close(io)

  lfortran_path = joinpath(@__DIR__, "lfortran")
  cmd = `$(lfortran_path) $code_path --show-ast --json`
  ast_json = JSON3.read(readchomp(cmd))

  return deserialize_ast(ast_json)
end
