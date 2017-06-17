module DolangTests

using Dolang
using Compat
using DataStructures
using Base.Test

tests = length(ARGS) > 0 ? ARGS : [
                                   "symbolic",
                                   "incidence",
                                   "factory",
                                   "compiler",
                                   "util",
                                   "printing",
                                   ]
for t in tests
    include("$(t).jl")
end

end
