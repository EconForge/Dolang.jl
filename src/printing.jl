# -------------- #
# latex printing #
# -------------- #
const latex_subs = let
    repl_latex = Base.REPLCompletions.latex_symbols
    unicode = Dict([(v, k) for (k, v) in repl_latex])
    tex_name = Dict([(strip(k, '\\'), k) for (k, v) in repl_latex])
    poppers = [k for k in keys(tex_name) if length(k)==1]
    for p in poppers
        pop!(tex_name, p)
    end
    merge(unicode, tex_name)
end

function _latex!(io::IO, v::Symbol, n::Union{Void,Integer}=nothing)
    # split the symbol into parts
    v_string = string(v)
    v_string = strip(v_string, '_')  # undo Dolang normalization
    parts = split(v_string, "_")

    # first part is the variable name, plus any overbar, dot, or star
    # let's extract all the modifiers
    front = parts[1]
    mods = []

    while true
        m = match(r"(?<front>\w+)(?<mod>bar|star|dot)", front)
        # if we didn't match, we are done
        m === nothing && break
        front = m["front"]
        push!(mods, m["mod"])
    end

    # now we can get the variable name
    var = get(latex_subs, front, front)

    # add mods
    "dot" in mods && print(io, "\\dot{")
    "bar" in mods && print(io, "\\overline{")

    # now add variable name and close dot/bar braces
    print(io, var)
    "dot" in mods && print(io, "}")
    "bar" in mods && print(io, "}")

    # now work on subscripts and superscripts. First get indices
    empty_ix = findfirst(isempty, parts)
    has_superscript = empty_ix > 0
    has_subscript = length(parts) > 1

    subscript_inds = 2:(has_superscript ? (empty_ix-1) : length(parts))
    superscript_inds = has_superscript ? (empty_ix+1:length(parts)) : (1:-1)

    # now print subscripts
    (has_subscript || isa(n, Integer)) && print(io, "_{")
    if has_subscript
        for part_ix in subscript_inds[1:end-1]
            _latex!(io, parts[part_ix])
            print(io, ",")
        end
        _latex!(io, parts[subscript_inds[end]])
    end

    if isa(n, Integer)
        t = n > 0 ? print(io, "t+", n) :
        n < 0 ? print(io, "t-", -n) :
        print(io, "t")
    else
        # we have one too many commas here, delete one
        # io.size -= 1; io.ptr -= 1
    end

    (has_subscript || isa(n, Integer)) && print(io, "}")


    # now print superscripts
    (has_superscript || "star" in mods) &&  print(io, "^{")

    if "star" in mods
        print(io, "*")
        has_superscript && print(io, ",")
    end

    if has_superscript
        for part_ix in superscript_inds[1:end-1]
            _latex!(io, parts[part_ix])
            print(io, ",")
        end
        _latex!(io, parts[superscript_inds[end]])
    end
    (has_superscript || "star" in mods) &&  print(io, "}")
end

_latex!(io::IO, s::AbstractString) = _latex!(io, parse(s))

_latex!{T<:Integer}(io::IO, x::Tuple{Symbol,T}) = _latex!(io, x[1], x[2])

_latex!(io::IO, x::Number) = print(io, string(x))

function _latex!(io::IO, ex::Expr)
    if is_time_shift(ex)
        _latex!(io, ex.args[1], ex.args[2])
        return
    end

    if ex.head == :(=)
        _latex!(io, :($(ex.args[1]) == $(ex.args[2]) ))
        return
    end
    if ex.head == :call
        if ex.args[1] == :(==)
            _latex!(io, ex.args[2])
            print(io, " = ")
            _latex!(io, ex.args[3])
            return
        end
        f = ex.args[1]
        if f in ARITH_SYMBOLS
            if length(ex.args) == 2  # sometimes a unary negation
                print(io, f)
                _latex!(io, ex.args[2])
            elseif length(ex.args) == 3

                if f == :^
                    print(io, "\\left(")
                    _latex!(io, ex.args[2])
                    print(io, "\\right)")
                    print(io, f)
                    print(io, "{")
                    _latex!(io, ex.args[3])
                    print(io, "}")
                elseif f == :/
                    print(io, "\\frac{")
                    _latex!(io, ex.args[2])
                    print(io, "}{")
                    _latex!(io, ex.args[3])
                    print(io, "}")
                elseif f == :*
                    _latex!(io, ex.args[2])
                    print(io, " ")
                    _latex!(io, ex.args[3])
                else
                    _latex!(io, ex.args[2])
                    print(io, f)
                    _latex!(io, ex.args[3])
                end

            elseif length(ex.args) > 3  # chained + or *
                for arg in ex.args[2:end-1]
                    _latex!(io, arg)
                    print(io, f)
                end
                _latex!(io, ex.args[end])
            end
            return
        end

        # not arith
        print(io, "\\text{$f}\\left(")
        for arg in ex.args[2:end]
            _latex!(io, arg)
        end
        print(io, "\\right)")
        return
    end

    error("not sure what to do here")
end


function latex(ex::Union{Expr,Symbol})
    io = IOBuffer()
    _latex!(io, ex)
    takebuf_string(io)
end

latex(s::String) = latex(parse(s))
