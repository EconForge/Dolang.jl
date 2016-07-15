module Dolang

using MacroTools
using DataStructures
using Calculus
using Compat: view

import Base: ==

export make_method, Der, FunctionFactory

# come convenience methods
_replace_star_star(s::AbstractString) = replace(s, "**", "^")

_to_expr(x::Expr) = x
_to_expr(x::Union{Symbol,Number}) = Expr(:block, x)
_to_expr(x::AbstractString) = _to_expr(parse(_replace_star_star(x)))

# core files
include("parser.jl")
include("factory.jl")
include("util.jl")
include("compiler.jl")

end  # module
