# ---------- #
# Base Types #
# ---------- #

immutable VariableNotAllowed <: Exception
    eq::Expr
    bad_var::Symbol
end

typealias FlatArgs Vector{Tuple{Symbol,Int}}
typealias GroupedArgs Associative{Symbol,Vector{Tuple{Symbol,Int}}}
typealias ArgType Union{FlatArgs,GroupedArgs}

typealias FlatParams Vector{Symbol}
typealias GroupedParams Associative{Symbol,Vector{Symbol}}
typealias ParamType Union{FlatParams,GroupedParams}

immutable SkipArg end

function Base.convert(::Type{FlatArgs}, g::GroupedArgs)
    out = Array(Tuple{Symbol,Int}, 0)
    for (_, _vec) in g
        for i in _vec
            push!(out, i)
        end
    end
    out
end

Base.convert(::Type{FlatParams}, g::GroupedParams) =
    vcat([v for (k, v) in g]...)::Vector{Symbol}

function allowed_dates!(args::FlatArgs, out=Dict{Symbol,Set{Int}}())
    for (s, i) in args
        push!(get!(out, s, Set{Int}()), i)
    end
    out
end

allowed_dates(args::FlatArgs) = allowed_dates!(args)
allowed_dates(args::GroupedArgs) = allowed_dates(FlatArgs(args))
param_names(p::FlatParams) = p
param_names(p::GroupedParams) = param_names(FlatParams(p))

# ---------------- #
# helper functions #
# ---------------- #

is_time_shift(ex::Expr) = ex.head == :call &&
                          length(ex.args) == 2 &&
                          isa(ex.args[2], Integer)

function time_shift(s::Symbol, args::Vector{Symbol}, defs::Associative=Dict(),
                    shift::Int=0)

    # if `s` is a function arg then return parsed, shifted version of s
    if s in args
        return _parse((s, shift))
    end

    # if it is a def, recursively substitute it
    if haskey(defs, s)
        return time_shift(defs[s], args, defs, shift)
    end

    # any other symbols just gets parsed
    return _parse(s)
end

# let numbers through
time_shift(x::Number, args...) = x

function time_shift(ex::Expr, args::Vector{Symbol}, defs::Associative=Dict(),
                    shift::Int=0)
    # no shift, just return the parsed argument
    shift == 0 && return _parse(ex)

    # need to pattern match here to make sure we don't normalize function names
    @match ex begin
        var_(i_Integer) => return time_shift(var, args, defs, shift+i)
        f_(foo__) => begin
            out = Expr(:call, f)
            append!(out.args, [time_shift(_, args, defs, shift) for _ in foo])
            out
        end
        _ => Expr(ex.head, [time_shift(_, args, defs, shift) for _ in ex.args]...)
    end
end

subs(s::Symbol, d::Associative) = get(d, s, s)
subs(x::Number, d::Associative) = x
subs(ex::Expr, d::Associative) = Expr(ex.head, [subs(_, d) for _ in ex.args]...)

# -------------- #
# IncidenceTable #
# -------------- #

"""
Maps from equation number to a Dict: variable -> Set(time_periods)
"""
immutable IncidenceTable
    t::OrderedDict{Int,Dict{Symbol,Set{Int}}}
    by_var::Dict{Symbol,Set{Int}}
end

IncidenceTable() = IncidenceTable(OrderedDict(), Dict())

function IncidenceTable(eqs::Vector{Expr})
    # create incidence
    it = IncidenceTable()
    for (i, eq) in enumerate(eqs)
        visit!(it, eq, i, 0)
    end
    it
end

function IncidenceTable(eq::Expr)
    it = IncidenceTable()
    visit!(it, eq, 1, 0)
    it
end

Base.getindex(it::IncidenceTable, i::Int) = it.t[i]
Base.getindex(it::IncidenceTable, s::Symbol) = it.by_var[s]

function visit!(it::IncidenceTable, s::Symbol, n::Int, shift::Int)
    for_eq = get!(it.t, n, Dict{Symbol,Set{Int}}())
    for_sym = get!(for_eq, s, Set{Int}())
    push!(for_sym, shift)

    # update by_var
    push!(get!(it.by_var, s, Set{Int}()), shift)

    nothing
end

visit!(it::IncidenceTable, s::Number, n::Int, shift::Int) = nothing

function visit!(it::IncidenceTable, ex::Expr, n::Int, shift::Int)
    @match ex begin
        var_(i_Integer) => visit!(it, var, n, i+shift)
        f_(args__) => [visit!(it, _, n, shift) for _ in args]
        _ => [visit!(it, _, n, shift) for _ in ex.args]
    end
    nothing
end

# --------------- #
# FunctionFactory #
# --------------- #

immutable FunctionFactory{T1<:ArgType,T2<:ParamType,T3<:Associative,T4<:Type}
    eqs::Vector{Expr}
    args::T1
    params::T2
    targets::Vector{Symbol}
    defs::T3
    funname::Symbol
    dispatch::T4
    incidence::IncidenceTable

    function FunctionFactory(eqs, args, params, targets, defs, funname,
                             dispatch)
        # create incidence table of equations
        incidence = IncidenceTable(eqs)

        # construct table of dates each arg, param, target is allowed
        # start with args
        allowed_args = allowed_dates(args)

        allowed = copy(allowed_args)

        # now params and targets (they can only ever appear at 0)
        let
            s0 = Set([0])
            for p in param_names(params)
                allowed[p] = s0
            end

            for t in targets
                allowed[t] = s0
            end
        end

        # This maps from the _parsed_ name to the _parsed_ expression that
        # should be substituted for the parsed name
        def_map = Dict{Symbol,Expr}()
        a_names = collect(keys(allowed_args))

        # make sure the _used_ definitions are all valid. If they are, add
        # them to the def_map
        for (_def, _ex) in defs
            times = incidence[_def]

            def_incidence = IncidenceTable()

            # compute incidence of each shift
            for t in times
                visit!(def_incidence, _ex, 1, t)
            end

            # make sure appearance of each variable is allowed
            for (v, _set) in def_incidence.by_var
                _set ⊈ allowed[v] && error("invalid def")
            end

            # construct shifted version of the definition
            for t in times
                def_map[_parse((_def, t))] = time_shift(_ex, a_names, defs, t)
            end

            # Add these times to allowed map for `_def` so we can do equation
            # validation next
            allowed[_def] = Set(times)

        end

        # do equation validation
        for (i, _dict) in incidence.t
            for (v, _set) in _dict
                _set ⊈ allowed[v] && throw(VariableNotAllowed(eqs[i], v))
            end
        end

        # now normalize the equations and make subs
        normalized_eqs = [subs(_parse(eq, targets=targets), def_map) for eq in eqs]

        new(normalized_eqs, args, params, targets, defs, funname, dispatch,
            incidence)
    end
end

# default outer constructor to do inference and fill in type params
function FunctionFactory{T1,T2,T3,T4}(eqs::Vector{Expr}, args::T1, params::T2,
                                     targets::Vector{Symbol}, defs::T3,
                                     funname::Symbol, dispatch::T4)
    FunctionFactory{T1,T2,T3,T4}(eqs, args, params, targets, defs, funname,
                                 dispatch)
end

function FunctionFactory(eqs::Vector{Expr}, args::ArgType, params::ParamType;
                         targets=Symbol[], defs=Dict{Symbol,Any}(),
                         funname::Symbol=:anon)
    FunctionFactory(eqs, args, params, targets, defs, funname, SkipArg)
end

function FunctionFactory{T4}(::Type{T4}, eqs::Vector{Expr}, args::ArgType,
                             params::ParamType; targets=Symbol[],
                             defs=Dict{Symbol,Any}(), funname::Symbol=:anon)
    FunctionFactory(eqs, args, params, targets, defs, funname, T4)
end

_valid_symbols(x::Union{FlatArgs,FlatParams}) = x

_valid_symbols(x::Union{GroupedArgs}) =
    vcat([_valid_symbols(v) for (k, v) in x]...)::FlatArgs

_valid_symbols(x::Union{GroupedParams}) =
    vcat([_valid_symbols(v) for (k, v) in x]...)::FlatParams

_valid_variables(ff::FunctionFactory) = _valid_symbols(ff.args)

_valid_variables_raw(ff::FunctionFactory) = _valid_variables_raw(ff.args)

_valid_params(ff::FunctionFactory) = _valid_symbols(ff.params)

_valid_symbols(ff::FunctionFactory) =
    vcat(_valid_params(ff), _valid_variables(ff))

function is_valid(ff::FunctionFactory)
    # go through each equation
    for i in 1:length(ff.eqs)

        # and make sure only allowed symbols appear in incidence table
        eq_table = ff.incidence[i]

        for (v, dates) in eq_table
        end
    end
end
