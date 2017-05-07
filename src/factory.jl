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
        bad_exprs = [:($(v.bad_var)($shift)) for shift in v.shifts]
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
    bad_exprs = [:($(v.bad_var)($shift)) for shift in v.shifts]
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

@compat const FlatArgs =  Vector{Tuple{Symbol,Int}}
@compat const GroupedArgs =  Associative{Symbol,Vector{Tuple{Symbol,Int}}}
@compat const ArgType =  Union{FlatArgs,GroupedArgs}

@compat const FlatParams =  Vector{Symbol}
@compat const GroupedParams =  Associative{Symbol,Vector{Symbol}}
@compat const ParamType =  Union{FlatParams,GroupedParams}

immutable Der{T} end
@compat const TDer{n} = Type{Der{n}}

immutable SkipArg end

function Base.convert(::Type{FlatArgs}, g::GroupedArgs)
    out = Array{Tuple{Symbol,Int}}(0)
    for (_junk, _vec) in g
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

function IncidenceTable(eqs::AbstractVector, skip::Vector{Symbol}=Symbol[])
    # create incidence
    it = IncidenceTable()
    for (i, eq) in enumerate(eqs)
        visit!(it, eq, i, 0, skip)
    end
    it
end

function IncidenceTable(eq::Expr, skip::Vector{Symbol}=Symbol[])
    it = IncidenceTable()
    visit!(it, eq, 1, 0, skip)
    it
end

Base.getindex(it::IncidenceTable, i::Int) = it.by_date[i]
Base.getindex(it::IncidenceTable, s::Symbol) = it.by_var[s]

function visit!(it::IncidenceTable, s::Symbol, n::Int, shift::Int,
                skip::Vector{Symbol}=Symbol[])

    if s in skip
        return nothing
    end

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
function visit!(it::IncidenceTable, s::Any, n::Int, shift::Int,
                skip::Vector{Symbol}=Symbol[])
    nothing
end

function visit!(it::IncidenceTable, ex::Expr, n::Int, shift::Int,
                skip::Vector{Symbol}=Symbol[])
    if is_time_shift(ex)
        var = ex.args[1]
        i = ex.args[2]
        visit!(it, var, n, i+shift, skip)
        return nothing
    end

    # don't visit the function name
    if ex.head == :call
        for an_arg in ex.args[2:end]
            visit!(it, an_arg, n, shift, skip)
        end
        return nothing
    end

    # Otherwise just visit everything
    for an_arg in ex.args
        visit!(it, an_arg, n, shift, skip)
    end
    nothing
end

"Filter the args so that it only includes elements that appear in equations"
filter_args!(args::FlatArgs, incidence::IncidenceTable) =
    filter!(args) do x
        haskey(incidence.by_var, x[1]) && x[2] in incidence[x[1]]
    end

function filter_args!{T<:GroupedArgs}(args::T, incidence::IncidenceTable)
    return args

    # TODO: below is the code that handles this, but I don't think we want
    #       to remove things in the grouped version...
    out = T()
    for (k, v) in args
        out[k] = filter_args!(v, incidence)
    end
    out
end

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

    function (::Type{FunctionFactory{T1,T2,T3,T4}}){T1,T2,T3,T4}(
            eqs, args, params, targets, defs, funname, dispatch
        )

        # if there are no targets, we normalize equations immediately
        if isempty(targets)
            eqs = map(_rhs_only, eqs)
        end

        # Need FlatParams so `visit!` and `IncidenceTable` skip them when
        # visiting expressions
        _flat_params = FlatParams(params)

        # create incidence table of equations
        incidence = IncidenceTable(eqs, _flat_params)

        # construct table of dates each arg, param, target is allowed
        # start with args
        allowed_args = allowed_dates(args)
        allowed = copy(allowed_args)

        # now params and targets (they can only ever appear at 0)
        s0 = Set(0)
        for p in param_names(params)
            allowed[p] = s0
        end

        # NOTE: targets might also be in args, so we can't just set
        #       allowed[t]. we need to push onto a set if one exists
        for t in targets
            push!(get!(allowed, t, Set(0)), 0)
        end

        # This maps from the normalized_ name to the normalized_ expression that
        # should be substituted for the parsed name
        def_map = Dict{Symbol,Expr}()
        arg_nms = Set(keys(allowed_args))

        # build this empty set once and pass to time_shift below
        funcs = Set{Symbol}()

        # add all definitions at time 0 allowed (for when we have definitions
        # depend on one another)
        for _def in keys(defs)
            allowed[_def] = Set([0])
        end

        # NOTE: we need to make the keys of definitions have the form
        #       (def, shift)::Tuple{Symbol,Int} in order for csubs to work
        #       properly below
        norm_defs = OrderedDict{Tuple{Symbol,Int},Any}()
        for (_def, _ex) in defs
            norm_defs[(_def, 0)] = _ex
        end

        # make sure the _used_ definitions are all valid. If they are, add
        # them to the def_map
        for (_def, _ex) in defs
            if !haskey(incidence.by_var, _def)
                # if the definition never shows up, just move on :)
                continue
            end

            times = incidence[_def]

            # recursively resolve this defintion so we get down to args, params
            # and targets.
            _ex = csubs(_ex, norm_defs)

            # compute incidence of each shift and make sure it is valid
            for t in times
                def_incidence = IncidenceTable()

                visit!(def_incidence, _ex, 1, t, _flat_params)

                for (v, _set) in def_incidence.by_var
                    # make sure appearance of each variable is allowed
                    _check_known(allowed, v, _ex, Set(t))
                    if _set ⊈ allowed[v]
                        throw(DefinitionNotAllowedError(_def, _ex, t))
                    end
                end

                # construct shifted version of the definition and add to map
                k = normalize((_def, t))
                def_map[k] = normalize(time_shift(_ex, t, arg_nms, funcs, defs))

                # also add this definition to incidence. Need to loop over
                # incidence.by_eq to see which equations it appears in...
                for (equation_number, equation_incidence) in incidence.by_eq
                    if haskey(equation_incidence, _def)
                        visit!(incidence, _ex, equation_number, t, _flat_params)
                    end
                end
            end

            # Add these times to allowed map for `_def` so we can do equation
            # validation next
            push!(allowed[_def], times...)

        end

        # do equation validation
        for (i, _dict) in incidence.by_eq
            i == typemax(Int) && continue # we've already validated defs
            for (v, _set) in _dict
                _check_known(allowed, v, eqs[i])
                if _set ⊈ allowed[v]
                    shifts = setdiff(_set, allowed[v])
                    throw(VariableNotAllowedError(v, eqs[i], shifts))
                end
            end
        end

        # now normalize the equations and make subs
        _f(x) = _to_expr(csubs(normalize(x, targets=targets), def_map))
        normalized_eqs = [_f(eq) for eq in eqs]

        new{T1,T2,T3,T4}(normalized_eqs, args, params, targets, defs, funname,
                         dispatch, incidence)
    end
end

@compat const FFSkipArg{T1,T2,T3} = FunctionFactory{T1,T2,T3,Type{SkipArg}}

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
    all(i -> getfield(x1, i) == getfield(x2, i), fieldnames(x1))

nargs{T<:FlatArgs}(ff::FunctionFactory{T}) = length(ff.args)
nargs{T<:GroupedArgs}(ff::FunctionFactory{T}) =
    sum([length(v) for (_junk, v) in ff.args])::Int

nparams{T1,T2<:FlatParams}(ff::FunctionFactory{T1,T2}) = length(ff.param)
nparams{T1,T2<:GroupedParams}(ff::FunctionFactory{T1,T2}) =
    sum([length(v) for (_junk, v) in ff.params])::Int
