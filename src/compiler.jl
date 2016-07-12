# ----------------------- #
# Building up expressions #
# ----------------------- #

function _unpack_expr(names::Vector, rhs::Symbol)
    _args = [:($(_parse(names[i])) = Dolang._unpack_var($rhs, $i))
             for i in 1:length(names)]
    out = Expr(:block); out.args = _args; out
end

function _unpack_expr(d::Associative, _discard)
    _args = vcat([_unpack_expr(v, k) for (k, v) in d]...)
    out = Expr(:block); out.args = _args; out
end

_unpack_args(ff::FunctionFactory, vec::Symbol=:V) = _unpack_expr(ff.args, vec)

_unpack_params(ff::FunctionFactory, vec::Symbol=:p) =
    _unpack_expr(ff.params, vec)

"returns an expression `:(lhs[i] = rhs)`"
_assign_var_expr(lhs, rhs, i) = :(Dolang._assign_var($lhs, $rhs, $i))

"Evaluates main expressions in a function group and fills `out` with results"
function _equation_block(ff::FunctionFactory)
    n_expr = length(ff.eqs)
    func_block = Expr(:block)

    if isempty(ff.targets)
        # no targets, just set out = parsed_expr
        assignments = map((rhs, i) -> _assign_var_expr(:out, rhs, i),
                          ff.eqs, 1:n_expr)
        func_block.args = assignments
    else
        # otherwise, need to parse the targets, evaluate them, and then set
        # elements of out equal to the targets
        parsed_targets = map(_parse, ff.targets)
        assignments = map((rhs, i) -> _assign_var_expr(:out, rhs, i),
                          parsed_targets, 1:n_expr)
        func_block.args = vcat(ff.eqs, assignments)
    end

    return func_block
end

function _body_block(ff::FunctionFactory)
    _args = [_unpack_params(ff), _unpack_args(ff), _equation_block(ff),
             :(return out)]
    out = Expr(:block); out.args = _args; out
end

"Expression that allocates memory for variable `out` in non-mutating version"
_allocate_block(ff::FunctionFactory) =
    :(out = Dolang._allocate_out(eltype($(arg_names(ff)[1])), $(length(eqs)),
                          $(arg_names(ff)...)))

"Expression that checks the size of `out` in mutating version"
function _sizecheck_block(ff::FunctionFactory)
    ex = quote
        expected_size = Dolang._output_size($(length(ff.eqs)), $(arg_names(ff)...))
        if size(out) != expected_size
            msg = "Expected out to be size $(expected_size), found $(size(out))"
            throw(DimensionMismatch(msg))
        end
    end
    ex
end

arg_names{T1<:FlatArgs}(::FunctionFactory{T1}) = [:V]

# WARNING: This function will only work in a reliable way if the keys are
#          guaranteed to come out in the same order each time. To ensure this
#          we reccomend using an instance of `OrderedDict` from the
#          DataStructures.jl package instead of a `Dict` from Base.
arg_names{T1<:GroupedArgs}(ff::FunctionFactory{T1}) =
    collect(keys(ff.args))::Vector{Symbol}

param_names{T1,T2<:FlatParams}(::FunctionFactory{T1,T2}) = [:p]

param_names{T1,T2<:GroupedParams}(ff::FunctionFactory{T1,T2}) =
    collect(keys(ff.params))::Vector{Symbol}

"Method signature for non-mutating version of the function"
function signature(ff::FunctionFactory)
    last_args = vcat(arg_names(ff), param_names(ff))
    func_args = ff.dispatch == SkipArg ? last_args :
                                          vcat(:(::$(ff.dispatch)), last_args)
    out = Expr(:call, ff.funname)
    append!(out.args, func_args)
    out
end

"Method signature for mutating version of the function"
function signature!(ff::FunctionFactory)
    sig = signature(ff)

    # convert name to `!` version and insert `out` as first argument
    new_name = Symbol(sig.args[1], "!")
    splice!(sig.args, 1, [new_name, :out])

    sig
end

# NOTE: we could easily allocate and then call the mutating version, but we
#       don't do that because then we get overhead for allocating _and_
#       for checking the size of out
"Build allocating version of the method"
function _build_function(ff::FunctionFactory)
    func_body = Expr(:block, _allocate_block(ff), _body_block(ff))
    Expr(:function, signature(ff), func_body)
end

"Build non-allocating version of the method"
function _build_function!(ff::FunctionFactory)
    func_body = Expr(:block, _sizecheck_block(ff), _body_block(ff))
    Expr(:function, signature!(ff), func_body)
end

# -------- #
# User API #
# -------- #

function make_method(ff::FunctionFactory; mutating::Bool=true,
                     allocating::Bool=true)

    out = Expr(:block)
    mutating && push!(out.args, _build_function!(ff))
    allocating && push!(out.args, _build_function(ff))
    out
end

function make_method(eqs::Vector{Expr},
                     arguments::ArgType,
                     params::ParamType;
                     targets::Vector{Symbol}=Symbol[],
                     defs::Associative=Dict(),
                     funname::Symbol=:anonymous,
                     mutating::Bool=true,
                     allocating::Bool=true)
    ff = FunctionFactory(eqs, arguments, params,
                         targets=targets, defs=defs, funname=funname)

    make_method(ff; mutating=mutating, allocating=allocating)
end


eqs = [:(foo = log(a)+b/x), :(bar = c(1)+u*d(1))]
args = [(:a, 0), (:b, 0), (:c, 1), (:d, 1)]
params = [:u]
defs = Dict(:x=>:(a/(1-c(1))))
targets=[:foo, :bar]
ff = FunctionFactory(eqs, args, params, targets=targets, defs=defs)

# arguments1 = OrderedDict(:x=>[(:a, 0), (:b, 0)], :y=>[(:c, 1), (:d, 1)])
# params1 = OrderedDict(:p=>[:u])
# ff1 = FunctionFactory(eqs, arguments1, params1, targets=[:foo, :bar])
#
# ff1f = FunctionFactory(Float64, eqs, arguments1, params1, targets=[:foo, :bar])

ex = :(a + b - c/d | 0.0 <= c < 100)


@match :(a + b - c/d | 0.0 <= c < 100) begin
    $(Expr(:comparison, :__)) => true
    _ => false
end

ex

@match ex begin
    a_ | b_ => a, b
    _ => false
end
