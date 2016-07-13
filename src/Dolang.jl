module Dolang

using MacroTools
using DataStructures: OrderedDict
using Calculus
using Compat: view

import Base: ==

export make_method

include("parser.jl")
include("factory.jl")
include("util.jl")
include("compiler.jl")

end  # module
