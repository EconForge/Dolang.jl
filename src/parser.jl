# ----------------- #
# Parsing utilities #
# ----------------- #

call_expr(var, n) = n == 0 ? Symbol(var) :
                             Symbol(string(var, "_", n > 0 ? "_" : "m", abs(n)))

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

function _parse(ex::Expr; targets::Union{Vector{Expr},Vector{Symbol}}=Symbol[])
    @match ex begin
        # translate lhs = rhs  to rhs - lhs
        $(Expr(:(=), :__)) => eq_expr(ex, targets)

        # translate x(n) --> x__n_ and x(-n) -> x_mn_
        var_(shift_Integer) => _parse(call_expr(var, shift))

        # + and * can take multiple arguments. Need to slurp them
        +(a_, b_) => Expr(:call, :(.+), _parse(a), _parse(b))
        *(a_, b_) => Expr(:call, :(.*), _parse(a), _parse(b))
        +(a_, b_, c__) => _parse(Expr(:call, :(+), Expr(:call, :(.+), a, b), c...))
        *(a_, b_, c__) => _parse(Expr(:call, :(*), Expr(:call, :(.*), a, b), c...))

        # -, /, ^ can only take two at a time
        -(a_, b_) => Expr(:call, :(.-), _parse(a), _parse(b))
        /(a_, b_) => Expr(:call, :(./), _parse(a), _parse(b))
        ^(a_, b_) => Expr(:call, :(.^), _parse(a), _parse(b))

        # Other func calls. Just parse args. Allows arbitrary Julia functions
        f_(a__) => Expr(:call, f, map(_parse, a)...)

        # the bottom, just insert numbers and symbols
        x_Symbol_Number => x

        _ => error("Not sure what I just saw")
    end
end

_parse(s::AbstractString; kwargs...) = _parse(parse(s); kwargs...)

_parse(exs::Vector{Expr}; kwargs...) =
    Expr(:block, map(_ -> _parse(_; kwargs...), exs)...)

_parse(x::Tuple{Symbol,Int}) = _parse(call_expr(x))
