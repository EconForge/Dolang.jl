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

# # ---------- #
# # Base Types #
# # ---------- #

const FlatArgs = AbstractVector
const GroupedArgs = AbstractDict{Symbol,<:Any}
const ArgType = Union{FlatArgs,GroupedArgs}

const FlatParams = AbstractVector
const GroupedParams = AbstractDict{Symbol,<:Any}
const ParamType = Union{FlatParams,GroupedParams}

struct Der{T} end
const TDer{n} = Type{Der{n}}

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
construct a mapping between the time shifted stringified symbol for `k` to the
stringified expression that it should be replaced with.

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


    for def_var in keys(defs)
        if haskey(incidence.by_var, def_var)
            _ex = csubs(defs[def_var], norm_defs)
            for time in incidence.by_var[def_var]
                new_key = stringify((def_var, time))
                out[new_key] = stringify(
                    time_shift(_ex, time)
                )
            end
        end
    end
    out
end

# --------------- #
# FunctionFactory #
# --------------- #


SymExpr = Union{Expr,Symbol,Number}

struct FunctionFactory
        # stringified equations
        equations::OrderedDict{Symbol,SymExpr}
        # list of group of (stringified) variables
        arguments::OrderedDict{Symbol, Vector{Symbol}}
        # list of assigned variables
        # preamble: definitions
        preamble::OrderedDict{Symbol, SymExpr}
        # name of function
        funname::Symbol
end

const FlatFunctionFactory = FunctionFactory


# function FunctionFactory(equations::OrderedDict, arguments::OrderedDict, definitions::AbstractDict; eliminate_preamble=false, funname=:anon)

#     # eqs: OrderedDict (targets are keys)
#     # args: OrderedDict
#     # defs:
#     all_vars = union([list_variables(eq) for eq in values(equations)]...)
#     present_vars = union(union(values(arguments)...), keys(equations))
#     filter!(x->(x isa Tuple), present_vars)

#     # variables appearing in equations
#     needed_defs = setdiff(all_vars, present_vars)

#     defs = solve_definitions(definitions, needed_defs)
#     defs_stringified = OrderedDict{Symbol,SymExpr}([stringify(k)=>stringify(v) for (k,v) in defs])
#     args_stringified = OrderedDict{Symbol,Vector{Symbol}}([k=>stringify.(v) for (k,v) in arguments])
#     eqs_stringified = OrderedDict{Symbol, SymExpr}([stringify(k)=>stringify(v) for (k,v) in equations])
#     targets = [keys(eqs_stringified)...]
#     return FlatFunctionFactory( eqs_stringified, args_stringified, targets, defs_stringified, funname)

# end

# function FunctionFactory(equations::Vector, arguments, definitions; kwargs...)
#     eqs = OrderedDict(Symbol("eq_",i)=>eq for (i,eq) in enumerate(equations))
#     FlatFunctionFactory(eqs, arguments, definitions; kwargs...)
# end

# function FunctionFactory(equations, arguments::Vector, definitions; kwargs...)
#     args = OrderedDict(Symbol("arg_",i)=>arg for (i,arg) in enumerate(arguments))
#     FunctionFactory(equations, args, definitions; kwargs...)
# end

# function FunctionFactory(equations::Vector, arguments::Vector, definitions; kwargs...)
#     eqs = OrderedDict(Symbol("eq_",i)=>eq for (i,eq) in enumerate(equations))
#     args = OrderedDict(Symbol("arg_",i)=>arg for (i,arg) in enumerate(arguments))
#     FunctionFactory(eqs, args, definitions; kwargs...)
# end
