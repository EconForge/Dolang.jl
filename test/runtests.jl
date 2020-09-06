module DolangTests

using Dolang
using DataStructures
using StaticArrays
using SparseArrays
using Test
#
# tests = length(ARGS) > 0 ? ARGS : [
#                                    "symbolic",
#                                    "incidence",
#                                    "factory",
#                                    "compiler",
#                                    "compiler_new",
#                                    "util",
#                                    "printing",
#                                    ]
# for t in tests
#     include("$(t).jl")
# end

include("symbolic.jl")
include("incidence.jl")
# include("compiler_new.jl")
include("util.jl")
include("printing.jl")
include("minilanguage.jl")

end
