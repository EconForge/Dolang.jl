__precompile__(false)

module Dolang

using DataStructures


const ARITH_SYMBOLS = Set([:+, :-, :*, :/, :^])
const DOLANG_FUNCTIONS = Set([:sin, :cos, :tan, :exp, :log, :log10])
push!(DOLANG_FUNCTIONS, ARITH_SYMBOLS...)


using Compat: view, String, @compat

import Base: ==

export make_function, Der, FunctionFactory, normalize, is_time_shift, time_shift,
       subs, csubs, steady_state, list_symbols, list_symbols!, list_variables,
       list_parameters

# come convenience methods
_replace_star_star(s::AbstractString) = replace(s, "**", "^")

_to_expr(x::Expr) = x
_to_expr(x::Union{Symbol,Number}) = Expr(:block, x)
_to_expr(x::AbstractString) = _to_expr(parse(_replace_star_star(x)))

const HAVE_SYMENGINE = try
    eval(Dolang, :(import SymEngine))
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
         " than SymEngine.jl\n. To use SymEngine run the following code: ",
         "`Pkg.add(\"SymEngine\")`")
    deriv(eq, x) = Calculus.differentiate(eq, x)
    @inline prep_deriv(eq) = eq
    @inline post_deriv(eq) = eq
end

abstract type COrder end
abstract type FOrder end

# core files
include("symbolic.jl")
include("incidence.jl")
include("factory.jl")
include("util.jl")
include("compiler.jl")
include("printing.jl")


end  # module
