# latex
# -----

latex_subs = let
    repl_latex = Base.REPLCompletions.latex_symbols
    unicode = Dict([v => k for (k, v) in repl_latex])
    tex_name = Dict([strip(k, '\\') => k for (k, v) in repl_latex])
    merge(unicode, tex_name)
end

const

function latex(v::Symbol, n::Integer=0)
    subscript = n > 0 ? "_{t+$n}" :
                n < 0 ? "_{t-$n}" :
                "_{t}"
    v_string = string(v)
    var = get(latex_subs, v_string, v_string)
    "$(var)$(subscript)"
end

latex{T<:Integer}(x::Tuple{Symbol,T}) = latex(x[1], x[2])

latex(x::Number) = string(x)

function latex(ex::Expr)
    if is_time_shift(ex)
        latex(ex.args[1], ex.args[2])
    end

    out = Expr(ex.head)

    out.args = [latex(arg) for arg in ex.args]
    string(out)
end
