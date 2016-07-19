# ---------------- #
# Public interface #
# ---------------- #

# normalize
# ---------

immutable NormalizeError <: Exception
    ex::Expr
    msg::String
end

"""
```julia
normalize(ex::Expr; targets::Union{Vector{Expr},Vector{Symbol}}=Symbol[])
```

Determine if the expression has the form `var(n::Integer)`.
"""
is_time_shift(ex::Expr) = ex.head == :call &&
                          length(ex.args) == 2 &&
                          isa(ex.args[2], Integer)

"""
```julia
normalize(var::Union{String,Symbol}, n)
```

Normalize the string or symbol in the following way:

- if `n == 0` return `_var_`
- if `n > 0` return `_var__n_`
- if `n < 0` return `_var_mn_`

"""
function normalize(var::Union{String,Symbol}, n)
    n == 0 && return normalize(var)
    Symbol(string("_", var, "_", n > 0 ? "_" : "m", abs(n)), "_")
end

"""
```julia
normalize(x::Tuple{Symbol,Int})
```

Same as `normalize(x[1], x[2])`
"""
normalize(x::Tuple{Symbol,Int}) = normalize(x[1], x[2])

"""
```julia
normalize(x::Symbol)
```

Normalize the symbol by returning `_x_`
"""
normalize(x::Symbol) = Symbol(string("_", x, "_"))

"""
```julia
normalize(x::Number)
```

Just return `x`
"""
normalize(x::Number) = x

"""
```julia
normalize(ex::Expr; targets::Union{Vector{Expr},Vector{Symbol}}=Symbol[])
```

Recursively normalize `ex` according to the following rules (structure of list
below is `input form of ex: returned expression`):

- `lhs = rhs` and `targets` is not empty: `normalize(lhs) = normalize(rhs)`,
where `lhs` must be one of the symbols in `targets`
- `quote ex end`:  `normalize(ex)`
- `var(n::Integer)`: `normalize(var, n)`
- `a ⚡ b ⚡ c  ...` and `⚡` one of `+` or `*`: effectively return
`normalize(a) .⚡ normalize(b) .⚡ normalize(c)`
- `a ⚡ b` and `⚡` one of `-`, `/`, `^`, `+`, `*`: `normalize(a) ⚡ normalize(b)`
- `f(args...)`: `f(map(normalize, args)...)`
"""
function normalize(ex::Expr; targets::Union{Vector{Expr},Vector{Symbol}}=Symbol[])
    if ex.head == :(=)
        return eq_expr(ex, targets)
    end

    if ex.head == :block
        if length(ex.args) == 2 && isa(ex.args[1], LineNumberNode)
            return normalize(ex.args[2])
        end

        # often we have an Expr simliar to the above, except that we have filtered
        # out the line nodes. We end up with a block with one arg.
        # This is that case.
        if length(ex.args) == 1
            return normalize(ex.args[1])
        end
    end

    if ex.head == :call
        # translate x(n) --> x__n_ and x(-n) -> x_mn_
        if length(ex.args) == 2 && isa(ex.args[2], Integer)
            return normalize(ex.args[1], ex.args[2])
        end

        if ex.args[1] in (:+, :*)
            return call_plus_times_expr(ex)
        end

        if ex.args[1] in _arith_symbols
            if length(ex.args) == 2 # sometimes - might appear alone
                return Expr(:call, ex.args[1], normalize(ex.args[2]))
            end
            return Expr(:call, ex.args[1], normalize(ex.args[2]), normalize(ex.args[3]))
        end

        # otherwise it is just some random function call
        return Expr(:call, ex.args[1], map(normalize, ex.args[2:end])...)
    end

    throw(NormalizeError(ex, "Not sure what I just saw"))
end

"""
```julia
normalize(s::AbstractString; kwargs...)
```

Call `normalize(parse(s)::Expr; kwargs...)`
"""
normalize(s::AbstractString; kwargs...) = normalize(parse(s); kwargs...)

"""
```julia
normalize(exs::Vector{Expr}; kwargs...)
```

Construct a `begin`/`end` block formed by calling `normalize(_; kwargs...)` for
all `_` in `exs`
"""
normalize(exs::Vector{Expr}; kwargs...) =
    Expr(:block, map(_ -> normalize(_; kwargs...), exs)...)


# time_shift
# ----------

# TODO: make `time_shift(x, args, shfit, defs) = time_shift(subs(x, defs), args, shift)`

"""
```julia
time_shift(s::Symbol, args::Vector{Symbol}, shift::Int=0, defs::Associative=Dict())
```

If `s` is in `args`, then return the expression `s(shift)`

If `s` is in `defs`, then return `time_shift(defs[s], args, defs, shift)`

Otherwise return `s`
"""
function time_shift(s::Symbol, args::Vector{Symbol}, shift::Int=0,
                    defs::Associative=Dict())

    # if `s` is a function arg then return shifted version of s
    if s in args
        return :($s($(shift)))
    end

    # if it is a def, recursively substitute it
    if haskey(defs, s)
        return time_shift(defs[s], args, shift, defs)
    end

    # any other symbols just gets parsed
    return s
end

"""
```julia
time_shift(x::Number, other...)
```

Return `x` for all values of `other`
"""
time_shift(x::Number, other...) = x

"""
```julia
time_shift(ex::Expr, args::Vector{Symbol}, shift::Int=0, defs::Associative=Dict())
```

Recursively apply a `time_shift` to `ex` according to the following rules based
on the form of `ex` (list below has the form "contents of ex: return expr"):

- `var(n::Integer)`: `time_shift(var, args, shift + n, defs)`
- `f(other...)`: `f(map(_ -> time_shift(_, args, shift, defs), other))`
- Any other `Expr`: `Expr(ex.head, map(_ -> time_shift(_, args, shift, defs), ex.args))`
"""
function time_shift(ex::Expr, args::Vector{Symbol}, shift::Int=0,
                    defs::Associative=Dict())
    # no shift, just return the argument
    shift == 0 && return ex

    # need to pattern match here to make sure we don't normalize function names
    if is_time_shift(ex)
        var = ex.args[1]
        i = ex.args[2]
        return time_shift(var, args, shift+i, defs)
    end

    # if it is some kind of function call, shift arguments
    if ex.head == :call
        out = Expr(:call, ex.args[1])
        for arg in ex.args[2:end]
            push!(out.args, time_shift(arg, args, shift, defs))
        end
        return out
    end

    # otherwise just shift all args, but retain expr head
    out = Expr(ex.head)
    out.args = [time_shift(_, args, shift, defs) for _ in ex.args]
    return out
end

# subs
# ----

#=
_subs will always return a tuple `(new_item, did_change)`, where `new_item`
is the result of trying to do the sub and `did_change` is a bool specifying
whether or not any substitutions happened. This is used
=#
_subs(s::Symbol, d::Associative) = haskey(d, s) ? (d[s], true) : (s, false)
_subs(x::Number, d::Associative) = (x, false)
function _subs(ex::Expr, d::Associative)
    out_args = Array(Any, length(ex.args))
    changed = false

    for (i, arg) in enumerate(ex.args)
        new_arg, arg_changed = _subs(arg, d)
        out_args[i] = new_arg
        changed = changed || arg_changed
    end

    out = Expr(ex.head)
    out.args = out_args
    out, changed
end

"""
```julia
subs(ex::Union{Expr,Symbol,Number}, from::Symbol, to::Union{Symbol,Expr,Number})
```

Apply a substituion where all occurances of `from` in `ex` are replaced by `to`
"""
function subs(ex::Union{Expr,Symbol,Number}, from::Symbol,
              to::Union{Symbol,Expr,Number})
    _subs(ex, Dict(from=>to))[1]
end

"""
```julia
subs(ex::Union{Expr,Symbol,Number}, d::Associative)
```

Apply substituions to `ex` so that all keys in `d` are replaced by their values
"""
subs(ex::Union{Expr,Symbol,Number}, d::Associative) = _subs(ex, d)[1]

"""
```julia
recursive_subs(x::Union{Symbol,Number}, d::Associative)
```

If `x` is a key in `d`, return the assocaited value. Otherwise return `x`
"""
recursive_subs(x::Union{Symbol,Number}, d::Associative) = _subs(x, d)[1]

"""
```julia
recursive_subs(ex::Expr, d::Associative)
```

Recursively apply substitutions to `ex` such that all items that are a key
in `d` are replaced by their associated values. Different from `subs(x, d)`
in that definitions in `d` are allowed to depend on one another and will
all be fully resolved here.

## Example

```
ex = :(a + b)
d = Dict(:b => :(c/a), :c => :(2a))
subs(ex, d)  # returns :(a + c / a)
recursive_subs(ex, d)  # returns :(a + (2a) / a)
```
"""
function recursive_subs(ex::Expr, d::Associative)
    max_it = length(d) + 1
    max_it == 1 && return ex

    for i in 1:max_it
        ex, changed = _subs(ex, d)
        !changed && return ex
    end

    error("Could not resolve expression recursively.")
end


# --------- #
# Utilities #
# --------- #

const _arith_symbols = (:+, :-, :*, :^, :/)

function call_plus_times_expr(ex::Expr)
    fun = ex.args[1]
    if length(ex.args) == 3
        return Expr(:call, fun, normalize(ex.args[2]), normalize(ex.args[3]))
    else
        call_plus_times_expr(Expr(:call, ex.args[1],
                                  Expr(:call, fun, ex.args[2], ex.args[3]),
                                  ex.args[4:end]...))
    end
end


function eq_expr(ex::Expr, targets::Union{Vector{Expr},Vector{Symbol}}=Symbol[])
    # translate lhs = rhs to rhs - lhs
    if isempty(targets)
      return Expr(:call, :(-), normalize(ex.args[2]), normalize(ex.args[1]))
    end

    # ensure lhs is in targets
    if !(ex.args[1] in targets)
      msg = string("Expected expression of the form `lhs = rhs` ",
                   "where `lhs` is one of $(targets)")
      throw(NormalizeError(ex, msg))
    end

    Expr(:(=), normalize(ex.args[1]), normalize(ex.args[2]))

end
