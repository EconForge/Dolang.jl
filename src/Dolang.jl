module Dolang

using DataStructures
using Parameters

const HAVE_SYMENGINE = try
    import SymEngine;
    true
catch
    false
end::Bool

if HAVE_SYMENGINE
    import SymEngine
    deriv(eq::SymEngine.Basic, x) = SymEngine.diff(eq, x)
    @inline prep_deriv(eq) = SymEngine.Basic(eq)
    @inline post_deriv(eq) = SymEngine.walk_expression(eq)
else
    import Calculus
    warn("Using Calculus.jl for symbolic differentiation. This will be slower",
         " than SymEngine.jl\n. To use SymEngine call Pkg.add(\"SymEngine\")")
    deriv(eq, x) = Calculus.differentiate(eq, x)
    @inline prep_deriv(eq) = eq
    @inline post_deriv(eq) = eq
end

const ARITH_SYMBOLS = Set([:+, :-, :*, :/, :^])
const DOLANG_FUNCTIONS = Set([:sin, :cos, :tan, :exp, :log, :log10])
push!(DOLANG_FUNCTIONS, ARITH_SYMBOLS...)


using Compat: view, String, @compat

import Base: ==, normalize

export make_method, Der, FunctionFactory, normalize, is_time_shift, time_shift,
       subs, csubs

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
include("printing.jl")

end  # module
