# ------------------ #
# Exception handling #
# ------------------ #

struct VariableNotAllowedError <: Exception
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

const FlatArgs = AbstractVector
@compat const GroupedArgs = AbstractDict{Symbol,<:Any}
const ArgType = Union{FlatArgs,GroupedArgs}

const FlatParams = AbstractVector
@compat const GroupedParams = AbstractDict{Symbol,<:Any}
const ParamType = Union{FlatParams,GroupedParams}

struct Der{T} end
@compat const TDer{n} = Type{Der{n}}

struct SkipArg end

_to_flat(f::FlatArgs) = f
_to_flat(g::GroupedArgs) = vcat(values(g)...)

# ---------------- #
# helper functions #
# ---------------- #
"""
    build_definition_map(defs::AbstractDict, incidence::IncidenceTable,
                         dynvars::Vector{Symbol})

For each key `k` in `defs`, and each time shift in `incidence.by_eq[k]`,
construct a mapping between the time shifted normalized symbol for `k` to the
normalized expression that it should be replaced with.

## Example

```jlcon
julia> defs = Dict(:x=>:(a(0)/(1-c(1))));

julia> incidence = Dolang.IncidenceTable(:(foo(0) = log(a(0))+b(0)/x(-1)));

julia> Dolang.build_definition_map(defs, incidence)
Dict{Symbol,Union{Expr, Number, Symbol}} with 1 entry:
  :_x_m1_ => :(_a_m1_ / (1 - _c__0_))
```
"""
function build_definition_map(defs::AbstractDict, incidence::IncidenceTable)
    out = Dict{Symbol,Union{Symbol,Expr,Number}}()

    # NOTE: we need to make the keys of definitions have the form
    #       (def, shift)::Tuple{Symbol,Int} in order for csubs to work
    #       properly below
    norm_defs = OrderedDict{Tuple{Symbol,Int},Any}()
    for (_def, _ex) in defs
        norm_defs[(_def, 0)] = _ex
    end

    funcs = Set{Symbol}()

    for def_var in keys(defs)
        if haskey(incidence.by_var, def_var)
            _ex = csubs(defs[def_var], norm_defs)
            for time in incidence.by_var[def_var]
                new_key = normalize((def_var, time))
                out[new_key] = normalize(
                    time_shift(_ex, time, funcs)
                )
            end
        end
    end
    out
end

# --------------- #
# FunctionFactory #
# --------------- #

struct FunctionFactory{T1<:ArgType,T2<:ParamType,T3<:AbstractDict,T4<:Type}
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

    function (::Type{FunctionFactory{T1,T2,T3,T4}})(
            eqs, args, params, _targets, defs, funname, dispatch
        ) where T1 where T2 where T3 where T4

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
        def_map = build_definition_map(defs, incidence)

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

function Base.show(io::IO, ::MIME"text/plain", ff::FunctionFactory)
    print(io, "FunctionFactory\n")
    @printf io "%16s: %s\n" "name" ff.funname
    @printf io "%16s: %s\n" "# of equations" length(ff.eqs)
    @printf io "%16s: %s\n" "# of args" nargs(ff)
    @printf io "%16s: %s\n" "# of params" length(ff.params)
    @printf io "%16s: %s\n" "Has targets?" !isempty(ff.targets)
end

@compat const FFSkipArg{T1,T2,T3} = FunctionFactory{T1,T2,T3,Type{SkipArg}}

# default outer constructor to do inference and fill in type params
function FunctionFactory(eqs::Vector{Expr}, args::T1, params::T2,
                                     targets, defs::T3,
                                     funname::Symbol, dispatch::T4) where {T1,T2,T3,T4}
    FunctionFactory{T1,T2,T3,T4}(eqs, args, params, targets, defs, funname,
                                 dispatch)
end

function FunctionFactory(eqs::Vector{Expr}, args::ArgType, params::ParamType;
                         targets=Symbol[], defs=Dict{Symbol,Any}(),
                         funname::Symbol=:anon)
    FunctionFactory(eqs, args, params, targets, defs, funname, SkipArg)
end

function FunctionFactory(dispatch::Type{T4}, eqs::Vector{Expr},
                             args::ArgType, params::ParamType; targets=Symbol[],
                             defs=Dict{Symbol,Any}(), funname::Symbol=:anon) where T4
    FunctionFactory(eqs, args, params, targets, defs, funname, T4)
end

==(x1::T, x2::T) where {T<:Union{IncidenceTable,FunctionFactory}}=
    all(i -> getfield(x1, i) == getfield(x2, i), fieldnames(typeof(x1)))

nargs(ff::FunctionFactory{T}) where {T<:FlatArgs} = length(ff.args)
nargs(ff::FunctionFactory{T}) where {T<:GroupedArgs} =
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

"""
    FunctionFactory{T4}([dispatch::Type{T4}], eqs::Vector{Expr},
                         args::ArgType, params::ParamType; targets=Symbol[],
                         defs=Dict{Symbol,Any}(), funname::Symbol=:anon)

Construct a `FunctionFactory` that Core.evaluates `eqs` using `args` and `params`.

`args` and `params` can either be flat `Vector` of Dolang symbols (not just
julia `Symbols`), or an AbstractDict mapping from a grouped argument name, to a
list of symbols in that group. See examples below:

```julia
# if ...
args = Dict(:x => [(:a, 0), (:b, 0)], :X => [(:a, 1)])
params = [:beta, :delta]

# compiled function would have arguments ...
# funname(..., x, X, p)
# where length(x) = 2, length(X) = 1, length(p) = 2
```

```julia
# if ...
args = [(:a, 0), (:b, 0), (:a, 1)]
params = [:beta, :delta]

# compiled function would have arguments ...
# funname(..., V, p)
# where length(V) = 3, length(p) = 2
```

Optional function arguments have the following purposes:

- `funname`: instruct the Dolang compiler that the compiled function should
  have a particular name
- `targets`: If non-empty and the symbols listed in `targets` and `eqs`
  contains statements of the form `lhs = rhs` --
- `defs`: Recursively substitute definitions into `eqs` (see [`csubs`](@ref)
  for more info)
- `dispatch`: If this argument is passed, then the Dolang compiler will
  generate code for a function whose first argument must be an instance of type
  `T4`. This can be used to compile functions with the same `funname`, but
  different behavior based on the type of `dispatch`. Note that the argument to
  `FunctionFactory` must be the name of a type, not an instance of a type (e.g.
  `Float64` instead of `1.0`), but when calling the compiled code you must pass
  an instance instead of the name of the type (e.g. `funname(1.0, ...)` not
  `funname(Float64, ...)`)
"""
FunctionFactory



##########################
#  Flat function factory #
##########################

# a FlatFunctionFactory object contains only what is needed
# to compile functions.
# Everything is "normalized", i.e. no time-variables.
#
# "preamble" can contain variables needed to compute the "equations".
#
# Here is an example:
# Dolang.FlatFunctionFactory(
#     DataStructures.OrderedDict(
#         :_foo__0_=>:(log(_a__0_) + _b__0_ / (_a_m1_ / (1 - _c__0_))),
#         :_foo__1_=>(_c__1_ + _u_ * _d__1_)
#     ),  # equations
#     DataStructures.OrderedDict( # arguments
#         :x=>Symbol[:_a_m1_],
#         :y=>Symbol[:_a__0_, :_b__0_, :_c__0_],
#         :z=>Symbol[:_c__1_, :_d__1_],
#         :p=>Symbol[:_u_]),
#     Symbol[:_foo__0_, :_bar__0_], # outputs (redundant)
#     DataStructures.OrderedDict{Symbol,Expr}(), # preamble
#     :myfun #function name
# )

SymExpr = Union{Expr,Symbol,Number}

struct FlatFunctionFactory
        # normalized equations
        equations::OrderedDict{Symbol,SymExpr}
        # list of group of (normalized) variables
        arguments::OrderedDict{Symbol, Vector{Symbol}}
        # list of assigned variables
        targets::Vector{Symbol}   ### Redundant
        # preamble: definitions
        preamble::OrderedDict{Symbol, SymExpr}
        # name of function
        funname::Symbol
end

function FlatFunctionFactory(ff::FunctionFactory; eliminate_definitions=false)

    equations = Union{Expr,Symbol}[]
    for eq in ff.eqs
        # we remove lhs if it is there
        if eq.head == :(=)
            ee = eq.args[2]
        else
            ee = eq
        end
        push!(equations, ee)
    end

    arguments = OrderedDict{Symbol, Vector{Symbol}}()

    # we assume silently :p is not given as an argument in ff
    if isa(ff.args, OrderedDict)
        for k in keys(ff.args)
            arguments[k] = [Dolang.normalize(e) for e in ff.args[k]]
        end
    else
        arguments[:x] = [Dolang.normalize(e) for e in ff.args]
    end
    arguments[:p] = [Dolang.normalize(p) for p in ff.params]

    # we ignore definitions assuming they have already been substituted
    preamble = OrderedDict{Symbol, Expr}()

    if equations isa Vector
        if length(ff.targets)==0
            targets = [Symbol(string("outv_",i)) for i=1:length(ff.eqs)]
        else
            targets = ff.targets
        end
        eqs = OrderedDict(targets[i]=>eq for (i,eq) in enumerate(equations))
    else
        eqs = equations
    end

    FlatFunctionFactory(eqs, arguments, targets, preamble, ff.funname)

end


function FlatFunctionFactory(equations::OrderedDict, arguments::OrderedDict, definitions::AbstractDict; eliminate_preamble=false, funname=:anon)

    # eqs: OrderedDict (targets are keys)
    # args: OrderedDict
    # defs:
    all_vars = union([list_variables(eq) for eq in values(equations)]...)
    present_vars = union(union(values(arguments)...), keys(equations))
    filter!(x->(x isa Tuple), present_vars)

    # variables appearing in equations
    needed_defs = setdiff(all_vars, present_vars)

    defs = solve_definitions(definitions, needed_defs)

    defs_normalized = OrderedDict{Symbol,SymExpr}([normalize(k)=>normalize.(v) for (k,v) in defs])
    args_normalized = OrderedDict{Symbol,Vector{Symbol}}([k=>normalize.(v) for (k,v) in arguments])
    eqs_normalized = OrderedDict{Symbol, SymExpr}([normalize(k)=>normalize(v) for (k,v) in equations])
    targets = [keys(eqs_normalized)...]
    return FlatFunctionFactory( eqs_normalized, args_normalized, targets, defs_normalized, funname)

end

function FlatFunctionFactory(equations::Vector, arguments, definitions; kwargs...)
    eqs = OrderedDict(Symbol("eq_",i)=>eq for (i,eq) in enumerate(equations))
    FlatFunctionFactory(eqs, arguments, definitions; kwargs...)
end

function FlatFunctionFactory(equations, arguments::Vector, definitions; kwargs...)
    args = OrderedDict(Symbol("arg_",i)=>arg for (i,arg) in enumerate(arguments))
    FlatFunctionFactory(equations, args, definitions; kwargs...)
end

function FlatFunctionFactory(equations::Vector, arguments::Vector, definitions; kwargs...)
    eqs = OrderedDict(Symbol("eq_",i)=>eq for (i,eq) in enumerate(equations))
    args = OrderedDict(Symbol("arg_",i)=>arg for (i,arg) in enumerate(arguments))
    FlatFunctionFactory(eqs, args, definitions; kwargs...)
end
