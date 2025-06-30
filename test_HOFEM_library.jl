ENV["HOFEM_LIB_PATH"] = "/mnt/c/Users/danie/Desktop/uc3m/HOFEM-AIRBUS_Julia_interface/source/test_HOFEM_library_features/libfake_HOFEM_library.so"
include("src/HOFEM_jl.jl")
using .HOFEM_jl
using .HOFEM_jl.Interfaces: fakeObject_t

fakeObject = fakeObject_t()
