using JSON3

# Dictionary mapping ASDL type names to Julia types
const ASDL_TYPE_MAP = Dict{String,Type}()

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
ASDL_TYPE_MAP["Location"] = Location

# Base struct for all AST nodes
struct ASTNode
  node::String
  fields::Dict{String,Any}
  loc::Location
end

abstract type DeclAttributeNode end

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
ASDL_TYPE_MAP["SimpleAttribute"] = SimpleAttributeNode

@enum DimensionType begin
  DimensionExpr
  DimensionStar
  AssumedRank
end

struct Dimension
  end_star::DimensionType
end
ASDL_TYPE_MAP["dimension"] = Dimension

struct AttrDimensionNode <: DeclAttributeNode
  dim::Vector{Dimension}
end
ASDL_TYPE_MAP["AttrDimension"] = AttrDimensionNode

struct AttrExtendsNode <: DeclAttributeNode
  name::String
end
ASDL_TYPE_MAP["AttrExtends"] = AttrExtendsNode

@enum AttrIntent begin
  In
  Out
  InOut
end

struct AttrIntentNode <: DeclAttributeNode
  intent::AttrIntent
end
ASDL_TYPE_MAP["AttrIntent"] = AttrIntentNode

abstract type ExprNode end

struct Num <: ExprNode
  n::Int32
end
ASDL_TYPE_MAP["Num"] = Num

struct Name <: ExprNode
  id::String
end
ASDL_TYPE_MAP["Name"] = Name

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
  value::Union{ExprNode,Nothing}
  type::KindItemType
end
ASDL_TYPE_MAP["kind_item"] = KindItemNode

struct AttrTypeNode <: DeclAttributeNode
  type::DeclType
  kind::Vector{KindItemNode}
  attr::Union{DeclAttributeNode,Nothing}
  name::Union{String,Nothing}
end
ASDL_TYPE_MAP["AttrType"] = AttrTypeNode

abstract type ProgramUnitNode end

struct ArgNode
  arg::String
end
ASDL_TYPE_MAP["arg"] = ArgNode

struct SubroutineNode <: ProgramUnitNode
  name::String
  args::Vector{ArgNode}
  attributes::Vector{DeclAttributeNode}
end
ASDL_TYPE_MAP["Subroutine"] = SubroutineNode


struct FunctionNode <: ProgramUnitNode
  name::String
  args::Vector{ArgNode}
  attributes::Vector{DeclAttributeNode}
  return_var::Union{ExprNode,Nothing}
end
ASDL_TYPE_MAP["Function"] = FunctionNode

abstract type UnitDecl2Node end

struct VarSymNode
  name::String
  dim::Vector{Dimension}
end
ASDL_TYPE_MAP["var_sym"] = VarSymNode

struct DeclarationNode <: UnitDecl2Node
  vartype::DeclAttributeNode
  attributes::Vector{DeclAttributeNode}
  syms::Vector{VarSymNode}
end
ASDL_TYPE_MAP["Declaration"] = DeclarationNode

struct DerivedTypeNode <: UnitDecl2Node
  name::String
  attrtype::Vector{DeclAttributeNode}
  items::Vector{UnitDecl2Node}
end
ASDL_TYPE_MAP["DerivedType"] = DerivedTypeNode

struct ModuleNode <: AbstractModule
  name::String
  decl::Vector{UnitDecl2Node}
  contains::Vector{ProgramUnitNode}
end
ASDL_TYPE_MAP["Module"] = ModuleNode

# Helper function to convert string to enum value
function string_to_enum(::Type{T}, s::String) where {T<:Enum}
  for value in instances(T)
    if string(value) == s
      return value
    end
  end
  error("Invalid enum value $s for type $T")
end

function deserialize_node(ast_json::JSON3.Object)
  node_type = ast_json["node"]
  fields = ast_json["fields"]

  # Get the concrete type for this node
  concrete_type = get(ASDL_TYPE_MAP, node_type, nothing)
  if isnothing(concrete_type)
    return nothing
  end

  # Get the field names of the concrete type first
  struct_fields = fieldnames(concrete_type)

  # Now recursively process only the fields that exist in the struct
  processed_fields = Dict{String,Any}()
  for field_name in struct_fields
    field_str = String(field_name)
    if haskey(fields, field_str)
      value = fields[field_str]
      field_type = fieldtype(concrete_type, field_name)

      if value isa JSON3.Object
        # Recursively process nested objects
        processed_fields[field_str] = deserialize_node(value)
      elseif value isa JSON3.Array
        # Process arrays - could be array of nodes or primitives
        if field_type isa Union && isempty(value)
          # handle optional fields (they are populated as empty arrays in the AST JSON)
          processed_fields[field_str] = nothing
        elseif eltype(field_type) <: Enum
          # Handle array of enums
          processed_fields[field_str] = map(item -> string_to_enum(eltype(field_type), item), value)
        else
          processed_fields[field_str] = map(item ->
              item isa JSON3.Object ? deserialize_node(item) : item,
            value)
        end
      else
        # For primitive values or enums, convert if needed
        if field_type <: Enum
          processed_fields[field_str] = string_to_enum(field_type, value)
        else
          processed_fields[field_str] = value
        end
      end
    end
  end

  # Create the concrete type instance using the processed fields
  try
    # Build arguments for the constructor in the order of the struct fields
    constructor_args = map(struct_fields) do field
      field_type = fieldtype(concrete_type, field)
      value = get(processed_fields, String(field), nothing)
      if isnothing(value) && field_type <: Vector
        return field_type()  # Returns empty vector of correct type
      end
      return value
    end

    # Create instance
    return concrete_type(constructor_args...)
  catch e
    @error "Failed to construct $node_type" exception = e
    rethrow(e)
  end
end
