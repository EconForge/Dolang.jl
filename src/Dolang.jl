module Dolang

using DataStructures

const HAVE_SYMENGINE = try
    import SymEngine;
    true
catch
    false
end::Bool

if HAVE_SYMENGINE
    import SymEngine
    deriv(eq::SymEngine.Basic, x) = SymEngine.diff(eq, x)
    prep_deriv(eq) = SymEngine.Basic(eq)
else
    import Calculus
    warn("Using Calculus.jl for symbolic differentiation. This will be slower",
         " than SymEngine.jl\n. To use SymEngine call Pkg.add(\"SymeEngine\")")
    deriv(eq, x) = Calculus.differentiate(eq, x)
    prep_deriv(eq) = eq
end

using Compat: view, String

import Base: ==

export make_method, Der, FunctionFactory, normalize, is_time_shift, time_shift,
       subs, recursive_subs

# come convenience methods
_replace_star_star(s::AbstractString) = replace(s, "**", "^")

_to_expr(x::Expr) = x
_to_expr(x::Union{Symbol,Number}) = Expr(:block, x)
_to_expr(x::AbstractString) = _to_expr(parse(_replace_star_star(x)))

# core files
include("symbolic.jl")
include("factory.jl")
include("util.jl")
include("compiler.jl")

end  # module
