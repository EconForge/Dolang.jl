module Dolang

using DataStructures
using StaticArrays

const ARITH_SYMBOLS = Set([:+, :-, :*, :/, :^])
const DOLANG_FUNCTIONS = Set([:sin, :cos, :tan, :exp, :log, :log10])
push!(DOLANG_FUNCTIONS, ARITH_SYMBOLS...)

import Base: ==
import REPL

using Printf
export make_function, Der, FunctionFactory, normalize, is_time_shift, time_shift,
       subs, csubs, steady_state, list_symbols, list_symbols!, list_variables,
       list_parameters

# come convenience methods
_replace_star_star(s::AbstractString) = replace(s, "**" => "^")

_to_expr(x::Expr) = x
_to_expr(x::Union{Symbol,Number}) = Expr(:block, x)
_to_expr(x::AbstractString) = _to_expr(Meta.parse(_replace_star_star(x)))


import SymEngine
deriv(eq::SymEngine.Basic, x) = SymEngine.diff(eq, x)
# somebody will probably object to it:
deriv(eq::Expr, x::Symbol) = Meta.parse(string(SymEngine.diff(SymEngine.Basic(eq), SymEngine.Basic(x))))
@inline prep_deriv(eq) = SymEngine.Basic(eq)
@inline post_deriv(eq) = SymEngine.walk_expression(eq)


abstract type COrder end
abstract type FOrder end

# core files
include("symbolic.jl")
include("incidence.jl")
include("factory.jl")
include("util.jl")
include("compiler.jl")
include("compiler_new.jl")

include("printing.jl")


end  # module
