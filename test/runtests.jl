module DolangTests

using Dolang
using Compat
using DataStructures


if VERSION >= v"0.5-"
    using Base.Test
else
    using BaseTestNext
    const Test = BaseTestNext
end

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
