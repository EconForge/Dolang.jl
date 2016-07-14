# ------------------ #
# Exception handling #
# ------------------ #

immutable UnknownSymbolError <: Exception
    bad_var::Symbol
    eq::Expr
    shifts::Set{Int}
end

function Base.showerror(io::IO, v::UnknownSymbolError)
    if !isempty(v.shifts)
        bad_exprs = [:($(v.bad_var)($_)) for _ in v.shifts]
        bad_str = join(bad_exprs, ", ", " and ")
        print(io, "Unknown symbol(s) $(bad_str) found in equation $(v.eq). ")
    else
        print(io, "Unknown symbol $(v.bad_var) found in equation $(v.eq). ")
    end
    print(io, "Try adding $(v.bad_var) to the list of args")
end

immutable VariableNotAllowedError <: Exception
    bad_var::Symbol
    eq::Expr
    shifts::Set{Int}
end

function Base.showerror(io::IO, v::VariableNotAllowedError)
    bad_exprs = [:($(v.bad_var)($_)) for _ in v.shifts]
    bad_str = join(bad_exprs, ", ", " and ")
    print(io, "Invalid symbol(s) $(bad_str) found in equation $(v.eq). ")
    print(io, "Try adding $(v.bad_var) to the list of args")
end

immutable DefinitionNotAllowedError <: Exception
    var::Symbol
    def::Expr
    shift::Int
end

function Base.showerror(io::IO, d::DefinitionNotAllowedError)
    print(io, "Invalid definition found for $(d.var) = $(d.def). ")
    print(io, "Cannot apply at shift $(d.shift) given argument restrictions")
end

# ---------- #
# Base Types #
# ---------- #

typealias FlatArgs Vector{Tuple{Symbol,Int}}
typealias GroupedArgs Associative{Symbol,Vector{Tuple{Symbol,Int}}}
typealias ArgType Union{FlatArgs,GroupedArgs}

typealias FlatParams Vector{Symbol}
typealias GroupedParams Associative{Symbol,Vector{Symbol}}
typealias ParamType Union{FlatParams,GroupedParams}

immutable Der{T} end
typealias TDer{n} Type{Der{n}}

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

    # if `s` is a function arg then return shifted version of s
    if s in args
        return :($s($(shift)))
    end

    # if it is a def, recursively substitute it
    if haskey(defs, s)
        return time_shift(defs[s], args, defs, shift)
    end

    # any other symbols just gets parsed
    return s
end

# let numbers through
time_shift(x::Number, args...) = x

function time_shift(ex::Expr, args::Vector{Symbol}, defs::Associative=Dict(),
                    shift::Int=0)
    # no shift, just return the argument
    shift == 0 && return ex

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
    by_eq::Dict{Int,Dict{Symbol,Set{Int}}}
    by_var::Dict{Symbol,Set{Int}}
    by_date::Dict{Int,Set{Symbol}}
end

IncidenceTable() = IncidenceTable(Dict(), Dict(), Dict())

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

Base.getindex(it::IncidenceTable, i::Int) = it.by_date[i]
Base.getindex(it::IncidenceTable, s::Symbol) = it.by_var[s]

function visit!(it::IncidenceTable, s::Symbol, n::Int, shift::Int)
    for_eq = get!(it.by_eq, n, Dict{Symbol,Set{Int}}())
    for_sym = get!(for_eq, s, Set{Int}())
    push!(for_sym, shift)

    # update by_var
    push!(get!(it.by_var, s, Set{Int}()), shift)

    # upate by_date
    push!(get!(it.by_date, shift, Set{Symbol}()), s)

    nothing
end

# don't worry about anything else
visit!(it::IncidenceTable, s::Any, n::Int, shift::Int) = nothing

function visit!(it::IncidenceTable, ex::Expr, n::Int, shift::Int)
    @match ex begin
        var_(i_Integer) => visit!(it, var, n, i+shift)
        f_(args__) => [visit!(it, _, n, shift) for _ in args]
        _ => [visit!(it, _, n, shift) for _ in ex.args]
    end
    nothing
end

"Filter the args so that it only includes elements that appear in equations"
filter_args!(args::FlatArgs, incidence::IncidenceTable) =
    filter!(x -> x[2] in incidence[x[1]], args)

filter_args!(args::GroupedArgs, incidence::IncidenceTable) =
    map(x -> filter_args!(x[2], incidence), args)

filter_args(args::ArgType, incidence::IncidenceTable) =
    filter_args!(deepcopy(args), incidence)

# --------------- #
# FunctionFactory #
# --------------- #

function _check_known(allowed::Associative, v::Symbol, ex::Expr,
                      shifts::Set{Int}=Set{Int}())
    haskey(allowed, v) && return
    throw(UnknownSymbolError(v, ex, shifts))
end

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

        # if there are no targets, we normalize equations immediately
        if isempty(targets)
            eqs = map(_normalize, eqs)
        end
        # create incidence table of equations
        incidence = IncidenceTable(eqs)

        # construct table of dates each arg, param, target is allowed
        # start with args
        allowed_args = allowed_dates(args)

        allowed = copy(allowed_args)

        # now params and targets (they can only ever appear at 0)
        let
            s0 = Set(0)
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

            # compute incidence of each shift and make sure it is valid
            for t in times
                def_incidence = IncidenceTable()
                visit!(def_incidence, _ex, 1, t)

                for (v, _set) in def_incidence.by_var
                    # make sure appearance of each variable is allowed
                    _check_known(allowed, v, _ex, Set(t))
                    if _set ⊈ allowed[v]
                        throw(DefinitionNotAllowedError(_def, _ex, t))
                    end
                end

                # construct shifted version of the definition and add to map
                k = _parse((_def, t))
                def_map[k] = _parse(time_shift(_ex, a_names, defs, t))
            end

            # Add these times to allowed map for `_def` so we can do equation
            # validation next
            allowed[_def] = Set(times)

        end

        # do equation validation
        for (i, _dict) in incidence.by_eq
            for (v, _set) in _dict
                _check_known(allowed, v, eqs[i])
                if _set ⊈ allowed[v]
                    shifts = setdiff(_set, allowed[v])
                    throw(VariableNotAllowedError(v, eqs[i], shifts))
                end
            end
        end

        # now normalize the equations and make subs
        normalized_eqs = [subs(_parse(eq, targets=targets), def_map) for eq in eqs]

        # now filter args  and keep only those that actually appear in the
        # equations
        args = filter_args(args, incidence)

        # also sort so order is all (_, 1) variables, then (_, 0), then (_, -1)
        sort!(args, by=x->x[2], rev=true)

        # TODO: filter incidence
        # filter incidence so all parameters are removed
        for p in params
            # remove from by_var
            haskey(incidence.by_var, p) && delete!(incidence.by_var, p)

            # remove from by_eq
            for (_, d) in incidence.by_eq
                haskey(d, p) && delete!(d, p)
            end
        end

        # remove from by_date
        for (_, _set) in incidence.by_date
            filter!(x-> !(in(x, params)), _set)
        end

        new(normalized_eqs, args, params, targets, defs, funname, dispatch,
            incidence)
    end
end

typealias FFSkipArg{T1,T2,T3} FunctionFactory{T1,T2,T3,Type{SkipArg}}

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

=={T<:Union{IncidenceTable,FunctionFactory}}(x1::T, x2::T) =
    all(_ -> getfield(x1, _) == getfield(x2, _), fieldnames(x1))

nargs{T<:FlatArgs}(ff::FunctionFactory{T}) = length(ff.args)
nargs{T<:GroupedArgs}(ff::FunctionFactory{T}) =
    sum([length(v) for (_, v) in ff.args])::Int

nparams{T1,T2<:FlatParams}(ff::FunctionFactory{T1,T2}) = length(ff.param)
nparams{T1,T2<:GroupedParams}(ff::FunctionFactory{T1,T2}) =
    sum([length(v) for (_, v) in ff.params])::Int
