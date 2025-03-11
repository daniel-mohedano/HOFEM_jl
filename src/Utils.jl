using SparseArrays
using LinearAlgebra

function rel_error(a, b)
  return LinearAlgebra.norm(a - b) / LinearAlgebra.norm(a)
end

