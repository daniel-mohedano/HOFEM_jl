# Fortran AST structures
# This file contains the Julia structures that represent the essential parts of the Fortran AST

using JSON3, StructTypes

# Basic types used in the AST
abstract type ASTNode end

# Common JSON node structure
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

struct NodeFields{T}
    fields::T
    loc::Location
end

StructTypes.StructType(::Type{NodeFields{T}}) where {T} = StructTypes.Struct()

struct Node{T}
    node::String
    fields::T
    loc::Location
end

StructTypes.StructType(::Type{Node{T}}) where {T} = StructTypes.Struct()

# Intermediate structures that match JSON format
struct KindItemJSON
    id::Union{String, Nothing}
    value::Union{Dict, Nothing}
    type::String
end

StructTypes.StructType(::Type{KindItemJSON}) = StructTypes.Struct()

struct DimensionJSON
    start::Union{Dict, Nothing}
    end_::Union{Dict, Nothing}
    end_star::String
end

StructTypes.StructType(::Type{DimensionJSON}) = StructTypes.Struct()

struct VarSymbolJSON
    name::String
    dim::Union{Vector{Node{DimensionJSON}}, Nothing}
    codim::Union{Vector{Dict}, Nothing}
    length::Union{Dict, Nothing}
    initializer::Union{Dict, Nothing}
end

StructTypes.StructType(::Type{VarSymbolJSON}) = StructTypes.Struct()

struct TypeAttributeJSON
    type::String
    kind::Union{Vector{Node{KindItemJSON}}, Nothing}
    name::Union{String, Nothing}
end

StructTypes.StructType(::Type{TypeAttributeJSON}) = StructTypes.Struct()

struct IntentAttributeJSON
    intent::String
end

StructTypes.StructType(::Type{IntentAttributeJSON}) = StructTypes.Struct()

struct ArgumentJSON
    arg::String
end

StructTypes.StructType(::Type{ArgumentJSON}) = StructTypes.Struct()

struct VariableDeclarationJSON
    vartype::Union{Node{TypeAttributeJSON}, Nothing}
    attributes::Union{Vector{Node}, Nothing}
    syms::Vector{Node{VarSymbolJSON}}
end

StructTypes.StructType(::Type{VariableDeclarationJSON}) = StructTypes.Struct()

struct FunctionJSON
    name::String
    args::Vector{Node{ArgumentJSON}}
    attributes::Union{Vector{Node}, Nothing}
    return_var::Union{Dict, Nothing}
end

StructTypes.StructType(::Type{FunctionJSON}) = StructTypes.Struct()

struct ModuleJSON
    name::String
    use::Vector{Node}
    decl::Vector{Node}
    contains::Vector{Node}
end

StructTypes.StructType(::Type{ModuleJSON}) = StructTypes.Struct()

# Conversion functions from JSON structures to our AST structures
function to_kind_item(json::KindItemJSON)::KindItem
    type = if json.type == "Value"
        Value
    elseif json.type == "Star"
        Star
    else
        Colon
    end
    KindItem(json.id, json.value === nothing ? nothing : FExpr(), type)
end

function to_dimension(json::DimensionJSON)::Dimension
    end_star = if json.end_star == "DimensionExpr"
        DimensionExpr
    elseif json.end_star == "DimensionStar"
        DimensionStar
    else
        AssumedRank
    end
    Dimension(
        json.start === nothing ? nothing : FExpr(),
        json.end_ === nothing ? nothing : FExpr(),
        end_star
    )
end

function to_var_symbol(json::VarSymbolJSON)::VarSymbol
    dimensions = if json.dim !== nothing
        [to_dimension(d.fields) for d in json.dim]
    else
        Dimension[]
    end
    
    VarSymbol(
        json.name,
        dimensions,
        Codimension[],
        nothing,
        nothing
    )
end

function to_type_attribute(json::TypeAttributeJSON)::TypeAttribute
    type = if json.type == "TypeInteger"
        TypeInteger
    elseif json.type == "TypeReal"
        TypeReal
    elseif json.type == "TypeComplex"
        TypeComplex
    elseif json.type == "TypeLogical"
        TypeLogical
    elseif json.type == "TypeCharacter"
        TypeCharacter
    else
        TypeType
    end
    
    kind = if json.kind !== nothing && !isempty(json.kind)
        to_kind_item(json.kind[1].fields)
    else
        nothing
    end
    
    TypeAttribute(type, kind, json.name)
end

function to_intent_attribute(json::IntentAttributeJSON)::IntentAttribute
    intent = if json.intent == "In"
        In
    elseif json.intent == "Out"
        Out
    else
        InOut
    end
    IntentAttribute(intent)
end

function to_variable_declaration(json::VariableDeclarationJSON)::VariableDeclaration
    vartype = if json.vartype !== nothing
        to_type_attribute(json.vartype.fields)
    else
        nothing
    end
    
    attributes = DeclAttribute[]
    if json.attributes !== nothing
        for attr in json.attributes
            if attr.node == "AttrIntent"
                push!(attributes, to_intent_attribute(JSON3.read(JSON3.write(attr.fields), IntentAttributeJSON)))
            end
        end
    end
    
    symbols = [to_var_symbol(sym.fields) for sym in json.syms]
    
    VariableDeclaration(vartype, attributes, symbols)
end

function to_argument(json::ArgumentJSON)::Argument
    Argument(json.arg, DeclAttribute[])
end

function to_function(json::FunctionJSON)::Function
    args = [to_argument(arg.fields) for arg in json.args]
    
    attributes = DeclAttribute[]
    if json.attributes !== nothing
        for attr in json.attributes
            if attr.node == "AttrType"
                push!(attributes, to_type_attribute(JSON3.read(JSON3.write(attr.fields), TypeAttributeJSON)))
            end
        end
    end
    
    return_var = json.return_var === nothing ? nothing : FExpr()
    
    Function(json.name, args, return_var, attributes)
end

function to_module(json::ModuleJSON)::Module
    uses = Use[]  # TODO: Implement conversion
    
    declarations = Declaration[]
    for decl in json.decl
        if decl.node == "Declaration"
            push!(declarations, to_variable_declaration(JSON3.read(JSON3.write(decl.fields), VariableDeclarationJSON)))
        end
    end
    
    contains = ProgramUnit[]
    for unit in json.contains
        if unit.node == "Function"
            push!(contains, to_function(JSON3.read(JSON3.write(unit.fields), FunctionJSON)))
        end
    end
    
    Module(json.name, uses, declarations, contains)
end

# Main deserialization function
function deserialize_ast(json::Dict)::Vector{Module}
    modules = Module[]
    if json["node"] == "TranslationUnit"
        items = json["fields"]["items"]
        for item in items
            if item["node"] == "Module"
                module_json = JSON3.read(JSON3.write(item["fields"]), ModuleJSON)
                push!(modules, to_module(module_json))
            end
        end
    end
    return modules
end

# Update build_ast function to use JSON3
function build_ast(code::String)::Vector{Module}
    code_path, io = mktemp()
    write(io, code)
    close(io)

    lfortran_path = joinpath(@__DIR__, "lfortran")
    cmd = `$(lfortran_path) $code_path --show-ast --json`
    ast_json = JSON3.read(readchomp(cmd))
    
    deserialize_ast(ast_json)
end