# ----------------- #
# Parsing utilities #
# ----------------- #

call_expr(var, n) = n == 0 ? Symbol(var, "_") :
                             Symbol(string(var, "_", n > 0 ? "_" : "m", abs(n)), "_")

call_expr(x::Tuple{Symbol,Int}) = call_expr(x[1], x[2])

function eq_expr(ex::Expr, targets::Union{Vector{Expr},Vector{Symbol}}=Symbol[])
    # translate lhs = rhs to rhs - lhs
    if isempty(targets)
        return Expr(:call, :(.-), _parse(ex.args[2]), _parse(ex.args[1]))
    end

    # ensure lhs is in targets
    if !(ex.args[1] in targets)
        msg = string("Expected expression of the form `lhs = rhs` ",
                     "where `lhs` is one of $(targets)")
        error(msg)
    end

    Expr(:(=), _parse(ex.args[1]), _parse(ex.args[2]))

end

_parse(x::Symbol) = Symbol(string(x, "_"))
_parse(x::Number) = x

const _dot_names = Dict{Symbol,Symbol}(:+ => :.+,
                                       :- => :.-,
                                       :* => :.*,
                                       :/ => :./,
                                       :^ => :.^,)
const _arith_symbols = (:+, :-, :*, :^, :/)

function call_plus_times_expr(ex::Expr)
    dot_func = _dot_names[ex.args[1]]

    if length(ex.args) == 3
        return Expr(:call, dot_func, _parse(ex.args[2]), _parse(ex.args[3]))
    else
        call_plus_times_expr(Expr(:call, ex.args[1],
                                  Expr(:call, dot_func, ex.args[2], ex.args[3]),
                                  ex.args[4:end]...))
    end
end

is_time_shift(ex::Expr) = ex.head == :call &&
                          length(ex.args) == 2 &&
                          isa(ex.args[2], Integer)


function _parse(ex::Expr; targets::Union{Vector{Expr},Vector{Symbol}}=Symbol[])
    if ex.head == :(=)
        return eq_expr(ex, targets)
    end

    if ex.head == :block && length(ex.args) == 2 && isa(ex.args[1], LineNumberNode)
        return _parse(ex.args[2])
    end

    if ex.head == :call
        # translate x(n) --> x__n_ and x(-n) -> x_mn_
        if length(ex.args) == 2 && isa(ex.args[2], Integer)
            return call_expr(ex.args[1], ex.args[2])
        end

        if ex.args[1] in (:+, :*)
            return call_plus_times_expr(ex)
        end

        if ex.args[1] in _arith_symbols
            if length(ex.args) == 2 # sometimes - might appear alone
                return Expr(:call, _dot_names[ex.args[1]], _parse(ex.args[2]))
            end
            return Expr(:call, _dot_names[ex.args[1]], _parse(ex.args[2]), _parse(ex.args[3]))
        end

        # otherwise it is just some random function call
        return Expr(:call, ex.args[1], map(_parse, ex.args[2:end])...)
    end

     error("Not sure what I just saw")
end

_parse(s::AbstractString; kwargs...) = _parse(parse(s); kwargs...)

_parse(exs::Vector{Expr}; kwargs...) =
    Expr(:block, map(_ -> _parse(_; kwargs...), exs)...)

_parse(x::Tuple{Symbol,Int}) = call_expr(x)
