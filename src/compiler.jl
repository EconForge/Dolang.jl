# --------- #
# Utilities #
# --------- #

_is_not_line(x) = true
_is_not_line(::LineNumberNode) = false
_is_not_line(ex::Expr) = ex.head != :line

_filter_lines!(x) = x

function _filter_lines!(ex::Expr)
    filter!(_is_not_line, ex.args)  # filter these args
    map(_filter_lines!, ex.args)    # then map over any nested Expr
    ex
end

"Convert lhs = rhs to rhs - lhs. Used in computation of derivatives"
_normalize(ex::Expr) =
    ex.head == :(=) ? Expr(:call, :(.-), ex.args[2], ex.args[1]) : ex

function _unpack_expr(names::Vector, rhs::Symbol)
    _args = [:($(_parse(names[i])) = Dolang._unpack_var($rhs, $i))
             for i in 1:length(names)]
    out = Expr(:block); out.args = _args; out
end

function _unpack_expr(d::Associative, _discard)
    _args = vcat([_unpack_expr(v, k) for (k, v) in d]...)
    out = Expr(:block); out.args = _args; out
end

"returns an expression `:(lhs[i] = rhs)`"
_assign_var_expr(lhs, rhs, i) = :(Dolang._assign_var($lhs, $rhs, $i))

# ----------------- #
# Expression Blocks #
# ----------------- #

#=
Each function is composed of multiple blocks. Each of these blocks is
associated with a function that can be overloaded to customize behavior

For a function that allocates memory for the output and returns the alloacted
array, we have the following blocks (in this order):

1. `allocate_block`: Allocates memory to hold the output of the evaluated
equations. Memory is bound to a variable named `out`
2. `param_block`: Unpacks items from the `params` field
3. `arg_block`: Unpacks items from the `arg` field
4. `equation_block`: uses the now locally defined variables from params and
args to evaluate the equations
5. `return_block`: Simply returns `out`

For a mutating function that populates a pre-allocated array with the value
of the function at specified values for the args and params we have:

1. `sizecheck_block`: Checks that the size of the `out` argument that was
passed into the function is conformable with the input args and parameters and
the equations.
2. `param_block`: Unpacks items from the `params` field
3. `arg_block`: Unpacks items from the `arg` field
4. `equation_block`: uses the now locally defined variables from params and
args to evaluate the equations
5. `return_block`: Simply returns `out`

In both cases steps 2-5 are the same and are called the `body_block`

The `allocate_block`, `size_checkblock`, and `equation_block` can all depend
on the order of derivative to be computed. For that reason, the corresponding
functions all have the signature `func{n}(::FunctionFactory, ::TDer{n})`. To
implement the body of a function higher order derivatives, you only need to
provide methods for these functions. Also, each of them has the second argument
defaulting to `Der{0}`, so calling `func(ff)` will return the 0th order
derivative (or level) version of that block.

=#

"Expression that allocates memory for variable `out` in non-mutating version"
allocate_block(ff::FunctionFactory, ::TDer{0}=Der{0}) =
    :(out = Dolang._allocate_out(eltype($(arg_names(ff)[1])), $(length(ff.eqs)),
                                 $(arg_names(ff)...)))

"Expression that checks the size of `out` in mutating version"
function sizecheck_block(ff::FunctionFactory, ::TDer{0}=Der{0})
    ex = quote
        expected_size = Dolang._output_size($(length(ff.eqs)), $(arg_names(ff)...))
        if size(out) != expected_size
            msg = "Expected out to be size $(expected_size), found $(size(out))"
            throw(DimensionMismatch(msg))
        end
    end
    _filter_lines!(ex)
    ex
end

param_block(ff::FunctionFactory, vec::Symbol=:p) =
    _unpack_expr(ff.params, vec)

arg_block(ff::FunctionFactory, vec::Symbol=:V) = _unpack_expr(ff.args, vec)

"Evaluates main expressions in a function group and fills `out` with results"
function equation_block(ff::FunctionFactory, ::TDer{0}=Der{0})
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

function body_block{n}(ff::FunctionFactory, ::TDer{n})
    _args = [param_block(ff), arg_block(ff), equation_block(ff, Der{n}),
             :(return out)]
    out = Expr(:block); out.args = _args; out
end

# ------------------- #
# Function Signatures #
# ------------------- #

#=
In addition to the function blocks dicussed above, we also need to know the
signature of each function so it can be defined.

The signature of the generated function for `ff::FunctionFactory` has the
following structure:

`ff.funname([DERIVATIVE], [DISPATCH], arg_names(ff)..., param_names(ff)...)`

Let's take it once piece at a time:

- `ff.funname` is the provided function name
- `DERIVATIVE` has the form `::Type{Dolang.Der{N}}`, where `N` is meant to
specify the order(s) of the derivative to be evaluated. This allows you to use
the same function name, but control which order of derivative is evaluated by
passing `Der{N}` as the first argument to `ff.funname`
- `DISPATCH` has the form `::Type{ff.dispatch}` where `ff.dispatch` should be
a Julia `DataType`. This is used to create many methods for same function (i.e.
mulitple versions of the function with the same name), but have them be
distinguishable to the Julia compiler. See example usage to see how it works.
By default `ff.dispatch` is set to `Dolang.SkipArg`. When
`ff.dispatch == SkipArg` when this is encountered, the compiler completely
skips the `[DISPATCH]` section of the signature
- `arg_names(ff)...` is simply the name of the arguments from `ff.args`. If
`ff.args` is a `Vector` (more specifically a `Dolang.FlatArgs`), then this will
be `[:V]`. If `ff.args` is some `Associative` structure, then this will be the
keys of that structure.
- `param_names(ff)` is the same as `arg_names(ff)`, but applied to the
`ff.params` field

We also need a signature for the mutating version of the signature. This has
the structure

`ff.funname!([DERIVATIVE], [DISPATCH], out, arg_names(ff)..., param_names(ff)...)`

Everything is the same as above, except that `ff.funname!` is now the original
function name with `!` appended to it and there is an additional `out`
argument. This is the array that should be filled with the evaluated equations
and always comes _after_ arguments that drive dispatch (`DERIVATIVE` and
`DISPATCH`), but _before_ args and params.

=#

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

_extra_args(ff::FunctionFactory, d::TDer{0}) =
    ff.dispatch == SkipArg ? Any[] : [:(::$(ff.dispatch))]

_extra_args{n}(ff::FunctionFactory, d::TDer{n}) =
    ff.dispatch == SkipArg ? Any[:(::TDer{$n})] : [:(::TDer{$n}), :(::$(ff.dispatch))]


"Method signature for non-mutating version of the function"
function signature{n}(ff::FunctionFactory, d::TDer{n}=Der{0})
    func_args = vcat(ff.funname, _extra_args(ff, d),
                     arg_names(ff), param_names(ff))

    out = Expr(:call)
    out.args = func_args
    out
end

"Method signature for mutating version of the function"
function signature!{n}(ff::FunctionFactory, d::TDer{n}=Der{0})
    sig = signature(ff, d)

    # convert name to `!` version and insert `out` as first argument
    sig.args[1] = Symbol(sig.args[1], "!")
    insert!(sig.args, length(_extra_args(ff, d))+2, :out)
    sig
end

# ---------------- #
# First derivative #
# ---------------- #

# first we need a couple of helper methods

# our parsed expressions have `.+` instead of +. This is kosher here beacuse
# we know all varaibles represent scalars. Calculus.jl can't make this
# assumption, so they don't offer derivative rules for broadcasting arithmetic.
# I'm being un-cool here and adding methods to their function. I shouldn't do
# this, but we are still in proof of concept stages so I'm friggin' doin' it!
for (dotfunsym, funsym) in [(:.+, :+), (:.-, :-), (:.*, :*),
                            (:./, :/), (:.^, :^)]
    @eval Calculus.differentiate(::SymbolParameter{$(Meta.quot(dotfunsym))}, args, wrt) =
        differentiate(SymbolParameter{$(Meta.quot(funsym))}(), args, wrt)
end

function _jacobian_expr_mat(ff::FunctionFactory{FlatArgs})
    # NOTE: I'm starting with the easy version, where I just differentiate
    #       with respect to all arguments in the order they were given. It
    #       would be better if I went though `ff.incidence.by_var` and only
    #       made columns for variables that appear in the equations
    eqs = ff.eqs
    args = ff.args
    neq = length(ff.eqs)
    nvar = length(ff.args)
    [differentiate(_normalize(eqs[i]), _parse(args[j])) for i=1:neq, j=1:nvar]
end

_output_size(ff::FunctionFactory, ::TDer{1}) =
    (length(ff.eqs), length(arg_names(ff)))

# Now fill in FunctionFactory API
function allocate_block(ff::FunctionFactory, ::TDer{1})
    expected_size = _output_size(ff, Der{1})
    :(out = Array(Float64, $(expected_size)))
end

function sizecheck_block(ff::FunctionFactory, ::TDer{1})
    expected_size = _output_size(ff, Der{1})
    ex = quote
        if size(out) != $expected_size
            msg = "Expected out to be size $($(expected_size)), found $(size(out))"
            throw(DimensionMismatch(msg))
        end
    end
    _filter_lines!(ex)
    ex
end

function equation_block(ff::FunctionFactory{FlatArgs}, ::TDer{1})
    # TODO: don't hard code to Float64
    expr_mat = _jacobian_expr_mat(ff)
    neq = size(expr_mat, 1)
    nvar = size(expr_mat, 2)

    # construct expressions that define the body of this function.
    # we need neq*nvar of them
    expr_args = Array(Expr, length(expr_mat))

    # To do this we use linear indexing tricks to access `out` and `expr_mat`.
    # Note the offset on the index to expr_args also (needed because allocating)
    # is the first expression in the block
    for ix in eachindex(expr_mat)
        expr_args[ix] = :(out[$(ix)] = $(expr_mat[ix]))
    end

    expr_args[end] = :(return out)

    out = Expr(:block)
    out.args = expr_args
    out
end

# -------------------------- #
# Putting functions together #
# -------------------------- #

# NOTE: we could easily allocate and then call the mutating version, but we
#       don't do that because then we get overhead for allocating _and_
#       for checking the size of out
"Build allocating version of the method"
function _build_function{n}(ff::FunctionFactory, d::TDer{n}=Der{0})
    func_body = Expr(:block, allocate_block(ff, d), body_block(ff, d))
    Expr(:function, signature(ff, d), func_body)
end

"Build non-allocating version of the method"
function _build_function!{n}(ff::FunctionFactory, d::TDer{n}=Der{0})
    func_body = Expr(:block, sizecheck_block(ff, d), body_block(ff, d))
    Expr(:function, signature!(ff, d), func_body)
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

function make_method{T}(::Type{T}, eqs::Vector{Expr},
                        arguments::ArgType,
                        params::ParamType;
                        targets::Vector{Symbol}=Symbol[],
                        defs::Associative=Dict(),
                        funname::Symbol=:anonymous,
                        mutating::Bool=true,
                        allocating::Bool=true)
    ff = FunctionFactory(T, eqs, arguments, params,
                         targets=targets, defs=defs, funname=funname)

    make_method(ff; mutating=mutating, allocating=allocating)
end
