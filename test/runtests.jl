module DolangTests

using Dolang
using Compat
using DataStructures
using Base.Test
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

# include("symbolic.jl")
# include("incidence.jl")
# include("factory.jl")
# include("compiler.jl")
include("compiler_new.jl")
# include("util.jl")
# include("printing.jl")

end
