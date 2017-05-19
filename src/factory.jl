# ------------------ #
# Exception handling #
# ------------------ #

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

# ---------- #
# Base Types #
# ---------- #

@compat const FlatArgs = AbstractVector
@compat const GroupedArgs = Associative{Symbol,<:Any}
@compat const ArgType = Union{FlatArgs,GroupedArgs}

@compat const FlatParams = AbstractVector
@compat const GroupedParams = Associative{Symbol,<:Any}
@compat const ParamType = Union{FlatParams,GroupedParams}

immutable Der{T} end
@compat const TDer{n} = Type{Der{n}}

immutable SkipArg end

_to_flat(f::FlatArgs) = f
_to_flat(g::GroupedArgs) = vcat(values(g)...)

# ---------------- #
# helper functions #
# ---------------- #
"""
    build_definition_map(defs::Associative, incidence::IncidenceTable,
                         dynvars::Vector{Symbol})

For each key `k` in `defs`, and each time shift in `incidence.by_eq[k]`,
construct a mapping between the time shifted normalized symbol for `k` to the
normalized expression that it should be replaced with.

## Example

```jlcon
julia> defs = Dict(:x=>:(a/(1-c(1))));

julia> incidence = Dolang.IncidenceTable(:(foo(0) = log(a)+b/x(-1)));

julia> dynvars = [:a, :b, :c];

julia> Dolang.build_definition_map(defs, incidence, dynvars)
Dict{Symbol,Union{Expr, Number, Symbol}} with 1 entry:
  :_x_m1_ => :(_a_m1_ / (1 - _c__0_))
```
"""
function build_definition_map(defs::Associative, incidence::IncidenceTable,
                              _dynvars::Union{Vector{Symbol},Set{Symbol}})
    out = Dict{Symbol,Union{Symbol,Expr,Number}}()

    # NOTE: we need to make the keys of definitions have the form
    #       (def, shift)::Tuple{Symbol,Int} in order for csubs to work
    #       properly below
    norm_defs = OrderedDict{Tuple{Symbol,Int},Any}()
    for (_def, _ex) in defs
        norm_defs[(_def, 0)] = _ex
    end

    funcs = Set{Symbol}()
    dynvars = Set(_dynvars)

    for def_var in keys(defs)
        if haskey(incidence.by_var, def_var)
            _ex = csubs(defs[def_var], norm_defs)
            for time in incidence.by_var[def_var]
                new_key = normalize((def_var, time))
                out[new_key] = normalize(
                    time_shift(_ex, time, dynvars, funcs, defs)
                )
            end
        end
    end
    out
end

# --------------- #
# FunctionFactory #
# --------------- #

immutable FunctionFactory{T1<:ArgType,T2<:ParamType,T3<:Associative,T4<:Type}
    # normalized equations
    eqs::Vector{Expr}
    # canonical  variables to differentiate wrt
    args::T1
    # canonical (not-normalized) variables not to differentiate wrt (e.g. parameters)
    params::T2
    # normalized target names
    targets::Vector{Symbol}
    # definitions to be (recursively) substituted into eqs
    defs::T3
    # name of function
    funname::Symbol
    # type to drive dispatch
    dispatch::T4
    # incidence table for the eqs
    incidence::IncidenceTable

    function (::Type{FunctionFactory{T1,T2,T3,T4}}){T1,T2,T3,T4}(
            eqs, args, params, _targets, defs, funname, dispatch
        )

        # if there are no _targets, we normalize equations immediately
        if isempty(_targets)
            eqs = map(_rhs_only, eqs)
        end
        targets = normalize.(_targets)

        # Need FlatParams so `visit!` and `IncidenceTable` skip them when
        # visiting expressions
        _flat_params = _to_flat(params)

        # create incidence table of equations
        incidence = IncidenceTable(eqs, _flat_params)

        # construct mapping from normalized definition name to desired
        # expression
        dynvars = arg_names(args)
        def_map = build_definition_map(defs, incidence, dynvars)

        # now normalize the equations and make subs
        # _f(x) = _to_expr(csubs(normalize(x, targets=targets), def_map))
        _f(x) = _to_expr(csubs(normalize(x, targets=targets), def_map))
        normalized_eqs = _f.(eqs)

        # also need to add definitions to incidence table so that derivative
        # code is correct
        norm_defs = OrderedDict{Tuple{Symbol,Int},Any}(
            (v, 0) => e for (v, e) in defs
        )
        for def_var in keys(defs)
            for (eq_num, eq_incidence) in incidence.by_eq
                if haskey(eq_incidence, def_var)
                    _ex = csubs(defs[def_var], norm_defs)
                    for t in eq_incidence[def_var]
                        visit!(incidence, _ex, eq_num, t, _flat_params)
                    end
                end
            end
        end

        ff = new{T1,T2,T3,T4}(
            normalized_eqs, args, params, targets, defs, funname, dispatch,
            incidence
        )
    end
end

@compat const FFSkipArg{T1,T2,T3} = FunctionFactory{T1,T2,T3,Type{SkipArg}}

# default outer constructor to do inference and fill in type params
function FunctionFactory{T1,T2,T3,T4}(eqs::Vector{Expr}, args::T1, params::T2,
                                     targets, defs::T3,
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
    sum(length(i) for i in values(ff.args))::Int

function validate!(ff::FunctionFactory)
    full_incidence = IncidenceTable(ff.eqs)
    known = vcat(normalize.(ff.args), normalize.(ff.params), ff.targets)

    for (eq_num, seen) in full_incidence.by_eq
        for variable in keys(seen)
            if !(in(variable, known))
                throw(VariableNotAllowedError(variable, ff.eqs[i], Set(0)))
            end
        end
    end
end
