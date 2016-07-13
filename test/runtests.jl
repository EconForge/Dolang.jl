module DoloTests

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
                                   "parser",
                                   "factory",
                                #    "compiler",
                                   ]
for t in tests
    include("$(t).jl")
end

end
