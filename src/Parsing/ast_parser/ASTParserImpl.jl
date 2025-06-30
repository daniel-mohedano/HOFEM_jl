include("ASTNodes.jl")

using JSON3

struct ASTParserImpl <: AbstractParser
end

function parse(p::ASTParserImpl, module_files::Vector{<:AbstractString})::Vector{<:AbstractModule}
  modules = ModuleNode[]

  for file in module_files
    if !(isfile(file))
      @error "Fortran module `$file` not found."
      continue
    end

    code = read(file, String)
    parsed_modules = parse_modules(code)
    append!(modules, parsed_modules)
  end

  return modules
end

function parse_modules(code::String)::Vector{ModuleNode}
  code_path, io = mktemp()
  write(io, code)
  close(io)

  lfortran_path = joinpath(@__DIR__, "lfortran")
  cmd = `$(lfortran_path) $code_path --show-ast --json`
  ast_json = JSON3.read(readchomp(cmd))

  return deserialize_ast(ast_json)
end


function deserialize_ast(ast_json::JSON3.Object)::Vector{ModuleNode}
  modules = ModuleNode[]

  if ast_json["node"] == "TranslationUnit"
    # Process each module in the translation unit
    for item in ast_json["fields"]["items"]
      if item["node"] == "Module"
        push!(modules, deserialize_node(item))
      end
    end
  end

  return modules
end

