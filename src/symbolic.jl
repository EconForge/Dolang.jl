using MacroTools: prewalk, postwalk
using MacroTools
import MacroTools


constants = [:π, :ℯ, :Inf]

# ----- #
# types #
# ----- #

struct stringifyError <: Exception
    ex::Expr
    msg::String
end

function stringifyError(ex::Expr)
    msg = """Dolang does not know how to stringify
    \t$(ex)
    """
    stringifyError(ex, msg)
end

function Base.showerror(io::IO, ne::stringifyError)
    print(io, ne.msg)
end

struct UnknownFunctionError <: Exception
    func_name::Symbol
    msg::String
end

# --------- #
# stringify #
# --------- #

"""
    stringify(var::Union{String,Symbol}, n::Integer)

stringify the string or symbol in the following way:

- if `n >= 0` return `_var__n_`
- if `n < 0` return `_var_mn_`

"""
function stringify(var::Union{String,Symbol}, n::Integer)
    Symbol(stringify(var), n >= 0 ? "_" : "m", abs(n), "_")
end

"""
    stringify(x::Tuple{Symbol,Integer})

Same as `stringify(x[1], x[2])`
"""
stringify(x::Tuple{Symbol,T}) where {T<:Integer} = stringify(x[1], x[2])

"""
    stringify(x::Symbol)

stringify the symbol by returning `_x_` if `x` doesn't alread have leading
and trailling `_` characters
"""
function stringify(x::Symbol)
    if x in constants
        return x
    end
    str_x = string(x)
    return Symbol(string("_", x, "_"))
end

# ---------- #
# time_shift #
# ---------- #


operators = [:+, :*, :^, :-, :/]


create_variable(sym, date::Int64) = date>0 ? :($sym[t+$date]) : date<0 ? :($sym[t-$(-date)]) : :($sym[t])


### sanitize expressions

# variable match
function match_var(expr)
    res = @match expr begin
        v_Symbol[t+d_Int64] => (v,d)
        v_Symbol[t-d_Int64] => (v,-d)
        v_Symbol[t] => (v,0)
        _ => return nothing
    end
    return res
end


# variable/symbol match
function match_var_flexible(expr)
    res = @match expr begin
        v_Symbol(d_Int64) => (v,d)
        v_Symbol[t+d_Int64] => (v,d)
        v_Symbol[t-d_Int64] => (v,-d)
        v_Symbol[t] => (v,0)
    end
    return res
end


function match_varfun(expr)
    res = @match expr begin
        v_Symbol(d_Int64) => (v,d)
        fun_Symbol(args__) => (fun,args)
        _ => return nothing
    end
    return res
end


function sanitize(expr::Expr; variables::Vector{Symbol}=Symbol[])
    m = match_var(expr) 
    if m !== nothing
        return stringify(m)
    end
    m = match_varfun(expr)
    if (m !== nothing)
        if (typeof(m) <: Tuple{Symbol, Int64}) & (m[1] in variables)
            return create_variable(m[1], m[2])
        else
            if m[1] in variables
                error("Incorrect subscript for variable ", m[1], " : ", m[2])
            # else
            #     return expr
            end
        end
    end
    head = expr.head
    args = expr.args
    new_args = []
    for a in args
        if a in variables
            push!(new_args, create_variable(a,0))
        else
            dig = sanitize(a; variables=variables)
            push!(new_args, dig)
        end
    end
    return Expr(head, new_args...)
end 

sanitize(s::Symbol; variables::Vector{Symbol}=Symbol[]) = s in variables ? create_variable(s,0) : s
sanitize(s; variables::Vector{Symbol}=Symbol[]) = s

compat_string(s::AbstractString) = replace(replace(replace(s, "|"=>"⟂"), "**"=>"^"), "inf"=>"Inf")

function parse_string(s::AbstractString; variables=Symbol[]) :: Union{Expr, Symbol}
    expr = sanitize(Meta.parse(compat_string(s)), variables=variables)
    # if typeof(expr) <: Symbol
    #     return :(+$expr)
    # else
        return expr
    # end
end

# function parse_equation(expr)
#     m = @match expr begin
#         # (lhs_ = rhs_  | lb_ <= x_ <= ub)_     => (lhs, rhs, lb, x, ub)
#         # (lhs_ == rhs_ | lb_ <= x_ <= ub)_     => (lhs, rhs, lb, x, ub)
#         # (lhs_ == rhs_ | lb_ <= x_ <= ub)_     => (lhs, rhs, lb, x, ub)
#         (lhs_ = rhs)                         => (lhs, rhs)
#         (lhs_ == rhs)                        => (lhs, rhs)
#         # (lhs_)                                => (lhs,)
#     end
#     return m
# end

function match_equation(expr)
    m = @match expr begin
        # lhs_ == rhs_                      => (lhs, rhs)
        lhs_ ⟂ lb_ <= x_ <= ub_     => (lhs, lb, x, ub)
        (lhs_ = rhs_)                       => (lhs, rhs)
        (lhs_ == rhs_)                       => (lhs, rhs)
        lhs_                                => (lhs,)
    end
    return m
end


function match_equality(expr)
    m = @match expr begin
        (lhs_ = rhs_)                       => (lhs, rhs)
        (lhs_ == rhs_)                       => (lhs, rhs)
        lhs_                                => (lhs,)
    end
    return m
end


###
###
###

# variable/symbol match
function match_expr(expr)
    res = @match expr begin
        # v_Symbol(d_Int64) => (v,d)
        v_Symbol[t+d_Int64] => (v,d)
        v_Symbol[t-d_Int64] => (v,-d)
        v_Symbol[t] => (v,0)
        v_Symbol => (v in operators ? nothing : v)
    end
    return res
end


###
### Basic symbolic operations
### 

struct SymbolList
    variables:: Vector{Tuple{Symbol, Int64}}
    parameters:: Vector{Symbol}
    functions:: Vector{Symbol}
end

function list_symbols(expr)

    funs = Symbol[]
    prewalk(u->(x=match_varfun(u);if x != nothing push!(funs, x[1]) end; u), expr)
    l = []
    prewalk(u->(x=match_expr(u);if x != nothing push!(l, x) else u end), expr)
    

    vars = [e for e in l if typeof(e) <: Tuple{Symbol, Int64}]

    parms = [e for e in l if (typeof(e) <: Symbol) & !(e in funs) & !(e in constants)]

    sl = SymbolList(unique(vars), unique(parms), unique([f for f in funs if !(f in operators)]))

    return sl
end

list_variables(expr) = list_symbols(expr).variables
list_parameters(expr) = list_symbols(expr).parameters

stringify(s::AbstractString) = stringify(parse_string(replace(s,"|", "⟂")))

function stringify(tree, parameters)
    function fun(u)
        a = match_expr(u)
        if a===nothing
            return u
        end
        if typeof(a) <: Symbol
            if a in parameters
                return stringify(a)
            else
                return a
            end
        end
        return stringify(a)
    end
    prewalk(fun, tree)
end

stringify(tree) = stringify(tree, list_parameters(tree))


time_shift(tree, Δt) = prewalk(u->(a=match_var(u); a === nothing ? u : create_variable(a[1],a[2]+Δt)),tree)
steady_state(tree) = prewalk(u->(a=match_var(u); a === nothing ? u : create_variable(a[1],0)),tree)




# ---- #
# subs #
# ---- #

function subs(expr, d::AbstractDict{Tuple{Symbol, Int64}, T}) where T
    # replace variables
    fun  = u -> (a = match_var(u); a === nothing ? u : (a in keys(d)) ? d[a] : u)
    postwalk(fun, expr)
end

subs(expr, pair::Pair{Tuple{Symbol,Int64}, Any}) = subs(expr, Dict(pair))

subs(expr, from, to) =  subs(expr, from=>to)


# function subs(expr, from, to)
#     fun  = u -> (a = match_var(u); a === nothing ? u : (a==from) ? to : u)
#     postwalk(fun, expr)
# end

# function subs(expr, d::AbstractDict{Tuple{Symbol, Int64}, T}) where T
#     # replace variables
#     res = expr
#     for (from, to) in d
#         res = subs(res, from, to)
#     end
# end


function tsubs(expr, d::AbstractDict{Tuple{Symbol, Int64}, T}) where T
    # replace symbol, with lags
    if Set([h[2] for h in keys(d)]) != Set([0])
        error("All variables to be replaced must be given at date t.")
    end
    fun  = u -> (a = match_var(u); a === nothing ? u : ((a[1],0) in keys(d)) ? time_shift(d[(a[1],0)], a[2]) : u)
    postwalk(fun, expr)
end

tsubs(expr, pair::Pair{Tuple{Symbol,Int64}, Any}) = tsubs(expr, from=>to)
tsubs(expr, from, to) = tsubs(expr, from=>to)



###########
#         #
###########

function csubs(expr, d::AbstractDict)
    dd = reorder_triangular_block(d)
    subs(expr, dd)
end

# ---------#
# arg_name #
# ---------#

function arg_name(s::Symbol)::Symbol
    # is the symbol already stringified?
    str_s = string(s)
    if str_s[1] == str_s[end] == '_'
        parts = match(r"^_(.+)_[m_]\d+_$", str_s)
        if parts === nothing
            return s
        else
            return Symbol(parts[1])
        end
    else
        return s
    end
end
arg_name(s::Tuple{Symbol,Int}) = s[1]
function arg_name(ex::Expr)::Symbol
    if is_time_shift(ex)
        return ex.args[1]
    else
        m = string(
            "Expression $(ex) does not look like a variable. ",
            "The only allowed expressions must satisfy `is_time_shift(ex)`"
        )
        error(m)
    end
end

function arg_time(s::Symbol)::Int
    # is the symbol already stringified?
    str_s = string(s)
    if str_s[1] == str_s[end] == '_'
        # yep, we are already stringified, need to extract the
        r = r"(m)?(\d+)_$"
        parts = match(r, str_s)
        if parts === nothing
            # not a match
            return 0
        else
            shift = Base.parse(Int, parts[2])
            return parts[1] === nothing ? shift : -shift
        end
    else
        return 0
    end
end
arg_time(s::Tuple{Symbol,Int}) = s[2]
function arg_time(ex::Expr)::Int
    if is_time_shift(ex)
        return ex.args[2]
    else
        m = string(
            "Expression $(ex) does not look like a variable. ",
            "The only allowed expressions must satisfy `is_time_shift(ex)`"
        )
        error(m)
    end
end

arg_name_time(s) = (arg_name(s), arg_time(s))

arg_names(s::AbstractVector) = arg_name.(s)
arg_names(s::AbstractDict) = vcat([arg_names(v) for v in values(s)]...)

# --------- #
# Utilities #
# --------- #

"""
```julia
is_time_shift(ex::Expr)
```

Determine if the expression has the form `var(n::Integer)`.
"""
is_time_shift(ex::Expr) = ex.head == :call &&
                          length(ex.args) == 2 &&
                          isa(ex.args[1], Symbol) &&
                          isa(ex.args[2], Integer)
